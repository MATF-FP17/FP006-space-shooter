module Game
  ( run
  ) where

import Constants
import GameState 
import GameDraw

import SpriteCache
import Player
import Asteroid
import Projectile
import Enemy
import SpriteAnimation
import SpriteText
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Set (Set, empty, insert, delete, member)
import Data.Map.Strict (Map)
import Data.List ((\\), deleteFirstsBy)
import Data.Function (on)
import System.Random (StdGen, mkStdGen, randomR)

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

-- UPDATE FUNCTIONS -- TODO: Combine all into one update function

-- | Update all game objects in GameState
updateObjectsInGame :: Float -> GameState -> GameState
updateObjectsInGame seconds game =
  game { player = updatePlayer (keysPressed game) seconds (player game)
       , enemies = map (updateEnemy seconds) (enemies game)
       , obstaclesAsteroids = map (updateAsteroid seconds) (obstaclesAsteroids game)
       , playerProjectiles = map (updateProjectile seconds) (playerProjectiles game)
       , enemyProjectiles = map (updateProjectile seconds) (enemyProjectiles game)
       }

-- | Collision between asteroids and projectiles
handleProjectilesAsteroidsCollision :: GameState -> GameState
handleProjectilesAsteroidsCollision game =
        game { obstaclesAsteroids = smallerAsteroidsFromBigger ++  Prelude.map (\x-> snd x) remaindRegularAsteroids 
             , playerProjectiles = Prelude.map (\x-> snd x) remaindProjectiles
             , player = addScoreToPlayer score (player game)
             }
        where
        asteroids = zip [1..] $ obstaclesAsteroids game
        projectiles =zip [1..] $ playerProjectiles game 
        asteroidProjectilesList = [(a,p) | a <- asteroids, p <- projectiles]
        asteroidProjectilesListFiltered = Prelude.filter (\x -> checkForAsteoridProjectileCollision  (snd ( fst x)) (snd ( snd x))) asteroidProjectilesList
        asteroidIndicesForRemove = returnAsteroidIndices asteroidProjectilesList asteroidProjectilesListFiltered
        projectileIndicesForRemove = returnProjectileIndices asteroidProjectilesList asteroidProjectilesListFiltered
        remaindRegularAsteroids = Prelude.filter (\x->  (elem (fst x) asteroidIndicesForRemove) == False) asteroids
        bigAsteroidsForRemoveIndices = Prelude.filter (\x->  (elem (fst x) asteroidIndicesForRemove) == True && aWidth (snd x) == widthAsteroidBig) asteroids
        bigAsteroidsForRemove = Prelude.map (\x -> (snd x)) bigAsteroidsForRemoveIndices
        gen = generator game
        (speedX1, gen') = randomR (lowestAsteroidSpeedX, highestAsteroidSpeedX) gen ::(Float, StdGen)
        (speedY1, gen'') = randomR (lowestAsteroidSpeedY,highestAsteroidSpeedY) gen' ::(Float, StdGen)
        (speedX2, gen''') = randomR (lowestAsteroidSpeedX, highestAsteroidSpeedX) gen'' ::(Float, StdGen)
        (speedY2, gen'''') = randomR (lowestAsteroidSpeedY,highestAsteroidSpeedY) gen''' ::(Float, StdGen)
        (deg, gen''''') = randomR (15,30) gen'''' ::(Float, StdGen)
        smallerAsteroidsFromBigger = Prelude.foldl (\acc x ->  (Asteroid (aPosition x) widthAsteroidSmall (speedX1,speedY1) deg (sAsteroidSpriteSmall (sprites game))) : (Asteroid (aPosition x) widthAsteroidSmall ((-speedX1),speedY2) deg (sAsteroidSpriteSmall (sprites game))) : acc) [] bigAsteroidsForRemove
        remaindProjectiles = Prelude.filter (\x-> (elem (fst x) projectileIndicesForRemove) == False) projectiles
        score = length asteroidIndicesForRemove * asteroidDestructionScore



checkForAsteoridProjectileCollision :: Asteroid -> Projectile -> Bool
checkForAsteoridProjectileCollision asteroid projectile = centerDistance > radiusSum
        where
        (ax, ay) = aPosition asteroid
        aw = aWidth asteroid / 2 --asteroid radius
        (px, py) = rPosition projectile
        pw = projectileRadius
        centerDistance = (ax-px)^2 + (ay-py)^2
        radiusSum = (aw + pw)^2

returnAsteroidIndices :: [((Int , Asteroid),(Int, Projectile))] -> [((Int , Asteroid),(Int, Projectile))]->[Int]
returnAsteroidIndices  unfilteredList filteredList = asteroidIndices
        where 
        asteroidsForRemoving = Prelude.filter (\x-> (elem x filteredList) == False) unfilteredList
        asteroidIndices = Prelude.foldl (\acc x -> (fst ( fst x)) : acc) [] asteroidsForRemoving

returnProjectileIndices :: [((Int , Asteroid),(Int, Projectile))] -> [((Int , Asteroid),(Int, Projectile))]->[Int]
returnProjectileIndices  unfilteredList filteredList = projectileIndices
        where 
        projectilesForRemoving = Prelude.filter (\x-> (elem x filteredList) == False) unfilteredList
        projectileIndices = Prelude.foldl (\acc x -> (fst (snd x)) : acc) [] projectilesForRemoving


-- | Checks if there is collision between given circle and rectangle
-- Arguments: Circle center -> Circle radius -> Rectangle center -> Rectangle
-- width and height
circleRectangleCollision :: (Float,Float) -> (Float) -> (Float, Float) -> (Float,Float) -> Bool
circleRectangleCollision (cx,cy) cr (rx,ry) (rw,rh) =
  if (distanceX > (rw2+cr)) then False
  else if (distanceY > (rh2+cr)) then False
  else if (distanceX <= rw2) then True
  else if (distanceY <= rh2) then True
  else ( (square(distanceX-rw2) + square(distanceY-rh2)) <= (square cr) )
  where 
    rw2 = rw / 2.0
    rh2 = rh / 2.0
    distanceX = abs (cx-rx)
    distanceY = abs (cy-ry)
    square :: Float -> Float
    square x = x * x

-- | Collision between asteroids and player
handlePlayerAsteroidsCollision :: GameState -> GameState
handlePlayerAsteroidsCollision game =
  game { player = damagePlayer totalDamage (player game) 
       , obstaclesAsteroids = notCollidedAsteroids
       }
  where
  asteroids = obstaclesAsteroids game
  notCollidedAsteroids = filter (not . (checkForPlayerAsteroidCollision (player game)) ) asteroids  
  totalDamage = ((length asteroids) - (length notCollidedAsteroids)) * asteroidDamageToPlayer

-- | Checks if there is collision between a single asteroid and player
-- Asteroid is treated as a circle and Player is treated as a rectangle
checkForPlayerAsteroidCollision :: PlayerState -> Asteroid -> Bool
checkForPlayerAsteroidCollision player asteroid = 
  circleRectangleCollision (ax,ay) ar (px,py) (pw,ph)
  where
    (ax, ay) = aPosition asteroid -- asteroid center
    ar = aWidth asteroid / 2 - spriteCorrection -- asteroid (circle) radius
    (px, py') = pPosition player
    py = (shipSizeHt - shipSizeHb) / 2 + py' -- actual center of spaceship rectangle
    pw = shipSizeWh * 2  -- spaceship's width
    ph = (shipSizeHt + shipSizeHb) -- spaceship's height
    spriteCorrection = 5 -- wiggle room for sprites not being perfect circle and rectangle


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


-- | Collision between enemies and projectiles
handleEnemiesProjectilesCollision :: GameState -> GameState
handleEnemiesProjectilesCollision game =
  game { enemies = map snd remainingEnemies
       , playerProjectiles = map snd remainingProjectiles
       , player = addScoreToPlayer score (player game)
       }
  where
    indexedEnemies = zip [1..] (enemies game)
    indexedProjectiles = zip [1..] (playerProjectiles game)
    pairsEnemiesProjectiles = [(e,p) | e <- indexedEnemies, p <- indexedProjectiles]
    collidedPairs = filter (\x -> checkForEnemiesProjectilesCollision (snd(fst x)) (snd (snd x))) pairsEnemiesProjectiles
    collidedEnemies = map fst collidedPairs
    collidedProjectiles = map snd collidedPairs 
    remainingEnemies = deleteFirstsBy sameIndex indexedEnemies collidedEnemies
    remainingProjectiles = deleteFirstsBy sameIndex indexedProjectiles collidedProjectiles
    score = ((length indexedEnemies) - (length remainingEnemies)) * enemyDestructionScore
    sameIndex :: (Int,a) -> (Int,a) -> Bool
    sameIndex (i1,_) (i2,_) = (i1==i2)


-- | Checks if there is collision between an enemy and a projectile
-- Projectile is treated as a circle and Enemy is treated as a rectangle
checkForEnemiesProjectilesCollision :: Enemy -> Projectile -> Bool
checkForEnemiesProjectilesCollision enemy projectile = 
  circleRectangleCollision (px,py) pr (ex,ey) (ew,eh)
  where
    (px,py) = rPosition projectile
    pr = projectileRadius
    (ex,ey) = ePosition enemy
    (ew,eh) = (enemySizeW - spriteCorrection, enemySizeH - spriteCorrection)
    spriteCorrection = 5 -- wiggle room for sprites not being perfect circle and rectangle

-- | Collision between player and enemy projectiles
handlePlayerProjectilesCollision :: GameState -> GameState
handlePlayerProjectilesCollision game = 
  game { player = damagePlayer totalDamage (player game)
       , enemyProjectiles = notCollidedProjectiles
       }
  where 
    projectiles = enemyProjectiles game
    notCollidedProjectiles = filter (not . (checkForPlayerProjectilesCollision (player game))) projectiles  
    totalDamage = ((length projectiles) - (length notCollidedProjectiles)) * enemyProjectileDamageToPlayer

-- | Checks if there is collision between the player and a projectile
-- Projectile is treated as a circle and Player is treated as a rectangle
checkForPlayerProjectilesCollision :: PlayerState -> Projectile -> Bool
checkForPlayerProjectilesCollision player projectile = 
  circleRectangleCollision (rx,ry) rr (px,py) (pw,ph)
  where
    (rx,ry) = rPosition projectile
    rr = projectileRadius
    (px, py') = pPosition player
    py = (shipSizeHt - shipSizeHb) / 2 + py' -- actual center of spaceship rectangle
    pw = shipSizeWh * 2 - spriteCorrection  -- spaceship's width
    ph = (shipSizeHt + shipSizeHb) - (spriteCorrection * 2) -- spaceship's height
    spriteCorrection = 5 -- wiggle room for sprites not being perfect circle and rectangle    

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


-- | Delete all game objects out of bounds from game
deleteObjectsFromGame :: GameState -> GameState
deleteObjectsFromGame game = 
  game { enemies = deleteOutOfBoundsEnemies (enemies game)
       , obstaclesAsteroids = deleteOutOfBoundsAsteroids (obstaclesAsteroids game)
       , playerProjectiles = deleteOutOfBoundsProjectiles (playerProjectiles game) 
       , enemyProjectiles = deleteOutOfBoundsProjectiles (enemyProjectiles game)
       }

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
