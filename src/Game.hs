module Game
  ( run
  ) where

import Constants
import GameState 
import GameDraw
import GameCollision
import SpriteCache (sProjectileSprites, sEnemySprites, sAsteroidSpriteSmall, sAsteroidSpriteBig, sSpriteFont)
import Player (PlayerState, updatePlayer, canPlayerFireProjectile, reloadPlayer, pScore, pPosition, pHealth)
import Asteroid (Asteroid(Asteroid), updateAsteroid, deleteOutOfBoundsAsteroids)
import Projectile (Projectile(Projectile), updateProjectile, deleteOutOfBoundsProjectiles, addProjectile)
import Enemy (Enemy(Enemy), updateEnemy,canEnemyFireProjectile, deleteOutOfBoundsEnemies, reloadEnemy, ePosition)
import SpriteAnimation (SpriteAnimation, makeRepeatingAnimation)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Set (insert, delete, member)
import Data.Function (on)
import System.Random (StdGen, randomR)

(/.) = (/) `on` fromIntegral -- divides two Integrals as Floats

window :: Display
window = InWindow "SpaceShooter" (width + iWidth, height) (offset, offset)

background :: Color
background = black

-- | Convert a game state into a picture.
render :: GameState  -- ^ The game state to render.
       -> Picture    -- ^ A picture of this game state.
render (WelcomeScreen _ sprites) = drawWelcomeScreen (sSpriteFont sprites)
render (GameOver _ sprites score) = drawGameOverScreen (sSpriteFont sprites) score
render game = drawGameScreen game

-- UPDATE FUNCTIONS

-- | Update all game objects in GameState
updateObjectsInGame :: Float -> GameState -> GameState
updateObjectsInGame seconds game =
  game { player = updatePlayer (keysPressed game) seconds (player game)
       , enemies = map (updateEnemy seconds) (enemies game)
       , obstaclesAsteroids = map (updateAsteroid seconds) (obstaclesAsteroids game)
       , playerProjectiles = map (updateProjectile seconds) (playerProjectiles game)
       , enemyProjectiles = map (updateProjectile seconds) (enemyProjectiles game)
       }

-- | Delete all game objects out of bounds from game
deleteObjectsFromGame :: GameState -> GameState
deleteObjectsFromGame game = 
  game { enemies = deleteOutOfBoundsEnemies (enemies game)
       , obstaclesAsteroids = deleteOutOfBoundsAsteroids (obstaclesAsteroids game)
       , playerProjectiles = deleteOutOfBoundsProjectiles (playerProjectiles game) 
       , enemyProjectiles = deleteOutOfBoundsProjectiles (enemyProjectiles game)
       }


-- | Add asteroids to the game
addAsteroidsToGame :: Float -> GameState -> GameState
addAsteroidsToGame seconds game =
    game { obstaclesAsteroids = newObstaclesAsteroids, generator = gen'''''' }
    where
    oldObstaclesAsteroids = obstaclesAsteroids game
    gen = generator game
    (x', gen') = randomR ((-width /.2 ) + wallBoundWidth + 32.0, (width /. 2) - wallBoundWidth - 32.0) gen :: (Float, StdGen) 
    y'= (height /. 2) - 2.0;
    (step,gen'') = randomR (1,600) gen' ::(Int, StdGen)
    (speedX, gen''') = randomR (lowestAsteroidSpeedX, highestAsteroidSpeedX) gen'' ::(Float, StdGen)
    (speedY, gen'''') = randomR (lowestAsteroidSpeedY,highestAsteroidSpeedY) gen''' ::(Float, StdGen)
    (deg, gen''''') = randomR (15,30) gen'''' ::(Float, StdGen)
    (smallbig, gen'''''') = randomR (1,6) gen''''' ::(Int, StdGen)
    newObstaclesAsteroids = if (step>597) 
                            then  if (smallbig <= 3) 
                                     then (Asteroid (x',y') widthAsteroidSmall (speedX,speedY) deg (sAsteroidSpriteSmall (sprites game))) : oldObstaclesAsteroids
                                     else (Asteroid (x',y') widthAsteroidBig (speedX,speedY) deg (sAsteroidSpriteBig (sprites game))) : oldObstaclesAsteroids
                            else oldObstaclesAsteroids

-- | Add enemies to the game
addEnemiesToGame :: Float -> GameState -> GameState
addEnemiesToGame seconds game =
  if (timeToAddNewEnemy game) - seconds > 0
  then game { timeToAddNewEnemy = (timeToAddNewEnemy game) - seconds }
  else game { enemies = newEnemy : (enemies game)
            , timeToAddNewEnemy = enemySpawnTime + (timeToAddNewEnemy game)
            , generator = gen'}
  where
    gen = generator game
    newEnemy = Enemy (ex,ey) (sx,sy) enemyInitialReloadTime (sEnemySprites (sprites game))
    enemyWidth = 20.0
    (ex,gen') = randomR ((-width /.2 ) + wallBoundWidth + enemyWidth, (width /. 2) - wallBoundWidth - enemyWidth) gen :: (Float, StdGen)
    ey = (height /. 2) - 1.0
    --(sx,gen'') = randomR (0,0) gen' :: (Float, StdGen)
    sx = 0
    (sy,gen'') = randomR (-75,-25) gen' :: (Float, StdGen)


-- | Player fired a projectile
projectileFiredByPlayer :: GameState -> GameState
projectileFiredByPlayer game = 
  if (  (member (SpecialKey KeySpace) (keysPressed game)) -- is the key for firing a projectile being held
     && (canPlayerFireProjectile (player game))  -- has the player reloaded a projectile
     )
  then -- fire
    game { playerProjectiles = addProjectile (px,py') (0,projectileSpeed) animation (playerProjectiles game)
         , player = reloadPlayer (player game) -- reload after firing  
         }
  else
    game
  where 
    (px,py) = pPosition (player game)
    py' = py + shipSizeHt
    animation :: SpriteAnimation
    animation = makeRepeatingAnimation projectileSpriteChangeInterval ((sProjectileSprites (sprites game))!!0)

-- | Enemies fired projectiles
projectilesFiredByEnemies :: GameState -> GameState
projectilesFiredByEnemies game =
  game { enemies = allReloaded
       , enemyProjectiles = newProjectiles ++ (enemyProjectiles game) 
       }
  where
  enemyList = enemies game
  enemiesReadyToFire = filter canEnemyFireProjectile enemyList
  newProjectiles = map (makeEnemyProjectile ((sProjectileSprites (sprites game))!!1)) enemiesReadyToFire
  allReloaded = map (\x -> if canEnemyFireProjectile x then reloadEnemy x else x) enemyList

-- | Make a projectile that will be fired by the enemy
makeEnemyProjectile :: [Picture] -> Enemy -> Projectile
makeEnemyProjectile sprites enemy = (Projectile (px,py) (sx,sy) animation) 
  where
  (px,py') = ePosition enemy
  py = py' - (enemySizeH / 2.0)
  (sx,sy) = (0,-projectileSpeed)
  animation :: SpriteAnimation
  animation = makeRepeatingAnimation projectileSpriteChangeInterval sprites


-- | Handle user input on WelcomeScreen
updateWelcomeScreen :: GameState -> GameState
updateWelcomeScreen game@(WelcomeScreen keysPressed loadedSprites) =
  if null keysPressed -- on any key pressed start the game
  then game 
  else initialLoadedGameState loadedSprites

-- | Handle user input on GameOver screen
updateGameOverScreen :: GameState -> GameState
updateGameOverScreen game@(GameOver keysPressed _ _) =
  if (member (Char 'r') keysPressed)
  then initialState 
  else game

-- | Handle user input on Game screen
handleInputGameScreen :: GameState -> GameState
handleInputGameScreen game = 
    if (member (Char 'p') (keysPressed game))
    then game { paused = not (paused game) 
              , keysPressed = delete (Char 'p') (keysPressed game) 
              }
    else if (member (Char 'r') (keysPressed game))
    then initialState
    else game

-- | Update the game
update :: Float -> GameState -> GameState 
update _ game@(WelcomeScreen _ _) = updateWelcomeScreen game
update _ game@(GameOver _ _ _) = updateGameOverScreen game
update seconds game = if (pHealth (player game)) <= 0
                      then (GameOver (keysPressed game) (sprites game) (pScore (player game)))
                      else if not (paused game) 
                      then 
                        handleInputGameScreen . --must be last
                        addEnemiesToGame seconds .
                        addAsteroidsToGame seconds .
                        projectilesFiredByEnemies .
                        projectileFiredByPlayer .
                        handlePlayerProjectilesCollision .
                        handleEnemiesProjectilesCollision .
                        handlePlayerAsteroidsCollision .
                        handleProjectilesAsteroidsCollision .
                        deleteObjectsFromGame .
                        updateObjectsInGame seconds $ 
                        game
                      else 
                        handleInputGameScreen game


-- INPUT FUNCTIONS --

-- | Respond to key events.
handleKeys :: Event -> GameState -> GameState

-- Remember all keys being pressed
handleKeys (EventKey key Down _ _) game =
  game { keysPressed = insert key (keysPressed game) }

-- Forget all keys not pressed anymore
handleKeys (EventKey key Up _ _) game =
  game { keysPressed = delete key (keysPressed game) }

-- Do nothing for all other events.
handleKeys _ game = game


run :: IO ()
run = play window background fps initialState render handleKeys update 
