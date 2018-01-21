module GameUpdate
  ( handleInputGameScreen
  , addEnemiesToGame
  , addAsteroidsToGame 
  , addHealthPackagesToGame 
  , addProjectilesFiredByEnemies 
  , addProjectileFiredByPlayer 
  , deleteObjectsFromGame 
  , updateObjectsInGame 
  , updateWelcomeScreen
  , updateGameOverScreen
  ) where
  
import Constants
import GameState 
import SpriteCache (sProjectileSprites, sEnemySprites, sAsteroidSpriteSmall, sAsteroidSpriteBig, sHealthImproveSprites)
import Player (updatePlayer, canPlayerFireProjectile, reloadPlayer, pScore, pPosition)
import Asteroid (Asteroid(Asteroid), updateAsteroid, deleteOutOfBoundsAsteroids)
import HealthPackage (HealthPackage(HealthPackage), updateHealthPackage, deleteOutOfBoundsHealthPackage)
import Projectile (Projectile(Projectile), updateProjectile, deleteOutOfBoundsProjectiles, addProjectile)
import Enemy (Enemy, initialEnemyState, updateEnemy, canEnemyFireProjectile, deleteOutOfBoundsEnemies, reloadEnemy, ePosition)
import SpriteAnimation (SpriteAnimation, makeRepeatingAnimation)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Set (delete, member)
import Data.Function (on)
import System.Random (StdGen, randomR)

(/.) :: Int -> Int -> Float
(/.) = (/) `on` fromIntegral -- divides two Integrals as Floats


-- | Update all game objects in GameState
updateObjectsInGame :: Float -> GameState -> GameState
updateObjectsInGame seconds game =
  game { player = updatePlayer (keysPressed game) seconds (player game)
       , enemies = map (updateEnemy seconds) (enemies game)
       , obstaclesAsteroids = map (updateAsteroid seconds) (obstaclesAsteroids game)
       , playerProjectiles = map (updateProjectile seconds) (playerProjectiles game)
       , enemyProjectiles = map (updateProjectile seconds) (enemyProjectiles game)
       , healthPackages = map (updateHealthPackage seconds) (healthPackages game)
       }

-- | Delete all game objects out of bounds from game
deleteObjectsFromGame :: GameState -> GameState
deleteObjectsFromGame game = 
  game { enemies = deleteOutOfBoundsEnemies (enemies game)
       , obstaclesAsteroids = deleteOutOfBoundsAsteroids (obstaclesAsteroids game)
       , playerProjectiles = deleteOutOfBoundsProjectiles (playerProjectiles game) 
       , enemyProjectiles = deleteOutOfBoundsProjectiles (enemyProjectiles game)
       , healthPackages = deleteOutOfBoundsHealthPackage (healthPackages game)
       }


       
-- | ADDING GAME OBJECTS

-- | Add asteroids to the game
addAsteroidsToGame :: Float -> GameState -> GameState
addAsteroidsToGame _ game =
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
            , generator = gen''}
  where
    gen = generator game
    newEnemy = initialEnemyState (ex,ey) (sx,sy) (sEnemySprites (sprites game))
    enemyWidth = 20.0
    (ex,gen') = randomR ((-width /.2 ) + wallBoundWidth + enemyWidth, (width /. 2) - wallBoundWidth - enemyWidth) gen :: (Float, StdGen)
    ey = (height /. 2) - 1.0
    sx = 0
    (sy,gen'') = randomR (-75,-25) gen' :: (Float, StdGen)

-- | Add healthPackages to the game
addHealthPackagesToGame :: Float -> GameState -> GameState
addHealthPackagesToGame _ game =
  if (currentScore > neededScore)
  then game { scoreForAddingHealthPackage = currentScore + scoreImprovementForHealthPackageAppearing
            , healthPackages = newPackage : (healthPackages game)
            , generator = gen''
            }
  else game 
  where
    currentScore = pScore $ player game
    neededScore = scoreForAddingHealthPackage game
    gen = generator game
    (x', gen') = randomR ((-width /.2 ) + wallBoundWidth + 32.0, (width /. 2) - wallBoundWidth - 32.0) gen :: (Float, StdGen) 
    y'= (height /. 2) - 2.0
    (speedY, gen'') = randomR (lowestHealthPackageSpeedY, highestHealthPackageSpeedY) gen' ::(Float, StdGen) -- gen'' is not used
    animation :: SpriteAnimation
    animation = makeRepeatingAnimation 0.2 (sHealthImproveSprites (sprites game))
    newPackage = HealthPackage (x',y') widthOfHealthImprove (0.0,speedY) animation
    

-- | Player fired a projectile
addProjectileFiredByPlayer :: GameState -> GameState
addProjectileFiredByPlayer game = 
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
addProjectilesFiredByEnemies :: GameState -> GameState
addProjectilesFiredByEnemies game =
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
makeEnemyProjectile spritesP enemy = (Projectile (px,py) (sx,sy) animation) 
  where
  (px,py') = ePosition enemy
  py = py' - (enemySizeH / 2.0)
  (sx,sy) = (0,-projectileSpeed)
  animation :: SpriteAnimation
  animation = makeRepeatingAnimation projectileSpriteChangeInterval spritesP


-- SCREEN/INPUT UPDATE FUNCTION
  
-- | Handle user input on WelcomeScreen
updateWelcomeScreen :: GameState -> GameState
updateWelcomeScreen game@(WelcomeScreen keysPressedSet loadedSprites) =
  if null keysPressedSet -- on any key pressed start the game
  then game 
  else initialLoadedGameState loadedSprites

-- | Handle user input on GameOver screen
updateGameOverScreen :: GameState -> GameState
updateGameOverScreen game@(GameOver keysPressedSet _ _) =
  if (member (Char 'r') keysPressedSet)
  then initialState 
  else game

-- | Handle user input on Game screen
handleInputGameScreen :: GameState -> GameState
handleInputGameScreen game = 
    if (member (Char 'p') (keysPressed game))
    then game { paused = not (paused game) 
              , keysPressed = delete (Char 'p') (keysPressed game) 
              }
    else if (member (Char 'o') (keysPressed game))
    then game { showDebug = not (showDebug game) 
              , keysPressed = delete (Char 'o') (keysPressed game) 
              }
    else if (member (Char 'r') (keysPressed game))
    then initialState
    else game