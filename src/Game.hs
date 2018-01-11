module Game
  ( run
  ) where

import Constants
import SpriteCache
import Player
import Asteroid
import Projectile
import SpriteAnimation
import SpriteText
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Set (Set, empty, insert, delete, member) --hiding (map, show, filter)
import Data.Map.Strict (Map)
import Data.List ((\\), deleteFirstsBy)
import Data.Function
import System.Random

(/.) = (/) `on` fromIntegral -- divides two Integrals as Floats

window :: Display
window = InWindow "SpaceShooter" (width + iWidth, height) (offset, offset)

background :: Color
background = black

-- | Data describing the state of the game. 
data GameState = Game
  { player :: PlayerState             -- state of the player
  -- enemies :: [Enemy]               -- list of enemies
  , obstaclesAsteroids :: [Asteroid]  -- list of obstcales
  , playerProjectiles :: [Projectile] -- list of projectiles fired
  -- enemyProjectiles :: [Projectile] -- list of projectiles fired
  -- ....
  -- timeFromLastAddedEnemy :: Float 
  , keysPressed :: Set Key            -- keeps track of all keys currently held down
  , paused :: Bool                    -- shows if the game is currently paused
  , generator :: StdGen               -- seed for random numbers
  , sprites :: SpriteCache            -- cache with all game spirtes
  } 
  | WelcomeScreen
  { keysPressed :: Set Key 
  , paused :: Bool 
  , sprites :: SpriteCache
  } 
  | GameOver
  { keysPressed :: Set Key
  , paused :: Bool 
  , sprites :: SpriteCache
  , score :: Int
  }
  --deriving Show                     -- TODO: debug output

-- | The starting state for the game.
initialState :: GameState
initialState = WelcomeScreen
  { keysPressed = empty
  , paused = False
  , sprites = loadAllSprites
  }

-- | Transfering loaded sprites into GameState
initialLoadedGameState :: SpriteCache -> GameState
initialLoadedGameState loadedSprites = Game  
   { player = Player (0,(-150)) 100 100 0 0 (sSpaceshipSprites loadedSprites) noMovement
  -- enemies = []
  -- obstacle = []
  , obstaclesAsteroids = []
  , playerProjectiles = []
  -- enemiesProjectiles = []
  -- timeFromLastAddedEnemy = 0
  , keysPressed = empty
  , paused = False
  , generator = mkStdGen(23456)
  , sprites = loadedSprites
  }

-- DRAW FUNCTIONS --

drawWelcomeScreen :: Map Char Picture -> Picture
drawWelcomeScreen spriteFont =
  pictures
  [ Scale 2 2 $ rowOrder (-2) $ centerText "SPACE SHOOTER" spriteFont
  , rowOrder 2 $ centerText "Press P to start" spriteFont
  ]
  where
    rowOrder :: Float -> Picture -> Picture
    rowOrder number = translate 0 (fromIntegral (-imageSpriteFontSize) * number)
    center :: Int -> Picture -> Picture
    center length = translate (-(fromIntegral ((length)*imageSpriteFontSize) / 2.0)) 0
    centerText :: [Char] -> Map Char Picture -> Picture
    centerText text font = center (length text) $ makeSpriteText text font
  
drawGameOverScreen :: Map Char Picture -> Int -> Picture
drawGameOverScreen spriteFont score = 
  pictures
  [ Scale 2 2 $ rowOrder (-2) $ centerText "GAME OVER" spriteFont
  , rowOrder 0 $ centerText ("Score: "++(show score)) spriteFont
  , rowOrder 2 $ centerText "Press R to restart" spriteFont
  ]
  where
    rowOrder :: Float -> Picture -> Picture
    rowOrder number = translate 0 (fromIntegral (-imageSpriteFontSize) * number)
    center :: Int -> Picture -> Picture
    center length = translate (-(fromIntegral ((length)*imageSpriteFontSize) / 2.0)) 0
    centerText :: [Char] -> Map Char Picture -> Picture
    centerText text font = center (length text) $ makeSpriteText text font 

drawInfo :: Map Char Picture -> Int -> Int -> Picture
drawInfo spriteFont health score = 
  pictures
  [ rowOrder 1 $ makeSpriteText ("Health:"++(show health)) spriteFont
  , rowOrder 2 $ makeSpriteText ("Score:"++(show score)) spriteFont
  --, rowOrder 3 $ makeSpriteText "" spriteFont
  ]
  where
    rowOrder :: Float -> Picture -> Picture
    rowOrder number = translate 0 (fromIntegral (-imageSpriteFontSize) * number)

drawControlsInfo :: Map Char Picture -> Picture
drawControlsInfo spriteFont = 
  --Scale 0.8 0.8 $ 
  pictures
  [ rowROrder 4 $ makeSpriteTextTight "CONTROLS" spriteFont
  , rowROrder 3 $ makeSpriteTextTight "Move :WASD" spriteFont
  , rowROrder 2 $ makeSpriteTextTight "Shoot:Space" spriteFont
  , rowROrder 1 $ makeSpriteTextTight "Pause:P" spriteFont
  , rowROrder 0 $ makeSpriteTextTight "Reset:R" spriteFont
  ]
  where
    rowROrder :: Float -> Picture -> Picture
    rowROrder number = translate 0 (fromIntegral (imageSpriteFontSize) * number)

drawDebugInfo :: GameState -> Picture
drawDebugInfo game = 
  Scale debugTextScale debugTextScale $ 
    color red $ 
      pictures 
      [ rowROrder 7 $ Text "DEBUG INFO"
      , rowROrder 6 $ Text $ "No. of asteroids: " ++ 
          show (length (obstaclesAsteroids game))
      , rowROrder 5 $ Text $ "No. of player's projectiles: " ++ 
          show (length (playerProjectiles game))
      , rowROrder 4 $ Text "Player:"
      , rowROrder 3 $ Text $ debugPlayerPosition (player game) 
      , rowROrder 2 $ Text $ debugPlayerSpeed (player game)
      , rowROrder 1 $ Text $ debugPlayerReloadTime (player game)
      ]
  where
    rowROrder :: Float -> Picture -> Picture
    rowROrder number = translate 0 (textHeight * number)
    
drawPauseScreen :: Map Char Picture -> Picture
drawPauseScreen spriteFont =
  centerText "PAUSED" spriteFont       
  where
    center :: Int -> Picture -> Picture
    center length = translate (-(fromIntegral ((length)*imageSpriteFontSize) / 2.0)) 0
    centerText :: [Char] -> Map Char Picture -> Picture
    centerText text font = center (length text) $ makeSpriteText text font
  
-- | Convert a game state into a picture.
render :: GameState  -- ^ The game state to render.
       -> Picture    -- ^ A picture of this game state.
render game@(WelcomeScreen _ _ sprites) = 
  pictures [drawWelcomeScreen (sSpriteFont sprites)]
render game@(GameOver _ _ sprites score) =
  pictures [drawGameOverScreen (sSpriteFont sprites) score]
render game =
  pictures 
  [ translate (iWidth /. 2) 0 $ pictures [walls,projectiles,spaceship,reloadBar, asteroids] -- all game objects
  , translateToInfoSideBar (height /. 2 ) $ drawInfo (sSpriteFont (sprites game)) (pHealth (player game)) (pScore (player game))
  , translateToInfoSideBar 0 $ drawControlsInfo (sSpriteFont (sprites game))
  , translateToInfoSideBar (-height /. 2 ) $ drawDebugInfo game
  , if (paused game) then translate (iWidth /. 2) 0 $ drawPauseScreen (sSpriteFont (sprites game)) else Blank
  ]
  where
    translateToInfoSideBar :: Float -> Picture -> Picture
    translateToInfoSideBar h = translate ((-width /. 2) + (-iWidth /. 2)) h
  
    -- player's spaceship
    spaceship :: Picture
    spaceship = drawSpaceShip (player game)
    
    reloadBar :: Picture
    reloadBar = drawReloadBar (player game)

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate offset 0 $
        color wallColor $
          rectangleSolid wallBoundWidth (fromIntegral height)

    wallColor = greyN 0.5
    walls = pictures [wall ((width /. 2) - (wallBoundWidth / 2)), 
                      wall (( (-width) /. 2) + (wallBoundWidth / 2))]
    
    -- Player projectiles
    projectiles :: Picture
    projectiles = pictures $ map drawProjectile (playerProjectiles game) 
    
    -- Asteroids
    asteroids :: Picture
    asteroids = pictures $ map drawAsteroid (obstaclesAsteroids game)


-- UPDATE FUNCTIONS -- TODO: Combine all into one update function
    
-- | Calls updatePlayer for GameState
updatePlayerInGame :: Float -> GameState -> GameState
updatePlayerInGame seconds game =
  game { player = updatePlayer (keysPressed game) seconds (player game) }

-- | Update all projectiles fired by player for GameState
updatePlayerProjectilesInGame :: Float -> GameState -> GameState
updatePlayerProjectilesInGame seconds game = 
  game { playerProjectiles = 
           map (updateProjectile seconds) (playerProjectiles game) }
 
 
-- | Update asteroids
updateAsteroidsInGame :: Float -> GameState -> GameState
updateAsteroidsInGame seconds game = 
        game { obstaclesAsteroids = Prelude.map (updateAsteroid seconds) (obstaclesAsteroids game) }

-- | Collision between asteroids and projectiles
handleProjectilesAsteroidsCollision :: GameState -> GameState
handleProjectilesAsteroidsCollision game =
        game { obstaclesAsteroids = Prelude.map (\x-> snd x) remaindAsteroids
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
        remaindAsteroids = Prelude.filter (\x->  (elem (fst x) asteroidIndicesForRemove) == False) asteroids
        remaindProjectiles = Prelude.filter (\x-> (elem (fst x) projectileIndicesForRemove) == False) projectiles
        score = length asteroidIndicesForRemove * asteroidDestructionScore
 
 

checkForAsteoridProjectileCollision :: Asteroid -> Projectile -> Bool
checkForAsteoridProjectileCollision asteroid projectile = centerDistance > radiusSum
        where
        (ax, ay) = aPosition asteroid
        aw = aWidth asteroid / 2 --asteroid radius
        (px, py) = rPosition projectile
        pw = 4 --projectile radius
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
  
  
-- | Collision between asteroids and player
handlePlayerAsteroidsCollision :: GameState -> GameState
handlePlayerAsteroidsCollision game =
  game { player = damagePlayer totalDamage (player game) 
       , obstaclesAsteroids = map snd notCollidedAsteroids
       }
  where
  asteroids = zip [1..] $ obstaclesAsteroids game
  notCollidedAsteroids = filter (not . (checkForPlayerAsteroidCollision (player game)) . snd) asteroids  
  totalDamage = ((length asteroids) - (length notCollidedAsteroids)) * asteroidDamageToPlayer

-- | Checks if there is collision between a single asteroid and player
-- Asteroid is treated as a circle and Player is treated as a rectangle
-- Implementation: Check if circle center (asteroid center) is inside Minkowski
-- sum of rectangle (player) and circle (asteroid)
checkForPlayerAsteroidCollision :: PlayerState -> Asteroid -> Bool
checkForPlayerAsteroidCollision player asteroid = 
  if (distanceX > (pw+ar)) then False
  else if (distanceY > (ph+ar)) then False
  else if (distanceX <= pw) then True
  else if (distanceY <= ph) then True
  else ( (square(distanceX-pw) + square(distanceY-ph)) <= (square ar) )
  where
    (ax, ay) = aPosition asteroid -- asteroid center
    ar = aWidth asteroid / 2 - spriteCorrection -- asteroid (circle) radius
    (px, py') = pPosition player
    py = (shipSizeHt - shipSizeHb) / 2 + py' -- actual center of spaceship rectangle
    pw = shipSizeWh  -- half of spaceship's width
    ph = (shipSizeHt + shipSizeHb) / 2 -- half of spaceship's height
    distanceX = abs(ax-px)
    distanceY = abs(ay-py)
    square :: Float -> Float
    square x = x * x
    spriteCorrection = 5 -- wiggle room for sprites not being perfect circle and rectangle


-- | Add asteroids to the game
addAsteroidsToGame :: Float -> GameState -> GameState
addAsteroidsToGame seconds game =
    game { obstaclesAsteroids = newObstaclesAsteroids, generator = gen''''' }
    where
    oldObstaclesAsteroids = obstaclesAsteroids game
    gen = generator game
    (x', gen') = randomR ((-width /.2 ) + wallBoundWidth + 32.0, (width /. 2) - wallBoundWidth - 32.0) gen :: (Float, StdGen) 
    y'= (height /. 2) - 2.0;
    (step,gen'') = randomR (1,600) gen' ::(Int, StdGen)
    (speedX, gen''') = randomR (lowestAsteroidSpeedX, highestAsteroidSpeedX) gen'' ::(Float, StdGen)
    (speedY, gen'''') = randomR (lowestAsteroidSpeedY,highestAsteroidSpeedY) gen''' ::(Float, StdGen)
    (deg, gen''''') = randomR (15,30) gen''' ::(Float, StdGen)
    newObstaclesAsteroids = if (step>598) 
                            then (Asteroid (x',y') 32.0 (speedX,speedY) deg (sAsteroidSprite (sprites game))) : oldObstaclesAsteroids
                            else oldObstaclesAsteroids

-- | Player fired a projectile
projectileFiredByPlayer :: GameState -> GameState
projectileFiredByPlayer game = 
  if (  (member (SpecialKey KeySpace) (keysPressed game)) -- is the key for firing a projectile being held
     && (canFireProjectile (player game))  -- has the player reloaded a projectile
     )
  then -- fire
    game { playerProjectiles = addProjectile (px,py') (0,projectileSpeed) animation (playerProjectiles game)
         , player = reload (player game) -- reload after firing  
         }
  else
    game
  where 
    (px,py) = pPosition (player game)
    py' = py + shipSizeHt
    animation :: SpriteAnimation
    animation = makeRepeatingAnimation projectileSpriteChangeInterval (sProjectileSprites (sprites game))

-- | Deletes all out of bounds projectiles from game
deleteProjectilesFormGame :: GameState -> GameState
deleteProjectilesFormGame game =
  game { playerProjectiles = 
           deleteOutOfBoundsProjectiles (playerProjectiles game) }

-- -- | Deletes all out of bounds asteroids from game
deleteAsteroidsFromGame :: GameState -> GameState
deleteAsteroidsFromGame game =
    game { obstaclesAsteroids = 
         deleteOutOfBoundsAsteroids (obstaclesAsteroids game) }


-- | Update the game
update :: Float -> GameState -> GameState 
update _ game@(WelcomeScreen _ _ loadedSprites) = 
  if not (paused game)
  then game
  else initialLoadedGameState loadedSprites
update _ game@(GameOver _ _ _ _) = game
update seconds game = if (pHealth (player game)) <= 0
                      then (GameOver (keysPressed game) False (sprites game) (pScore (player game)))
                      else if not (paused game) 
                      then 
                        projectileFiredByPlayer .
                        updatePlayerProjectilesInGame seconds . 
                        deleteProjectilesFormGame .
                        updateAsteroidsInGame seconds .
                        addAsteroidsToGame seconds .
                        deleteAsteroidsFromGame .
                        handlePlayerAsteroidsCollision .
                        handleProjectilesAsteroidsCollision .
                        updatePlayerInGame seconds $ 
                        game
                      else 
                        game




-- INPUT FUNCTIONS --

-- | Respond to key events.
handleKeys :: Event -> GameState -> GameState

-- For an 'p' keypress, pause the game
handleKeys (EventKey (Char 'p') Down _ _) game =
  game { paused = not (paused game) }

-- Ignore 'p' keypress release
handleKeys (EventKey (Char 'p') Up _ _) game = game

-- For an 'r' keypress, reset the game
handleKeys (EventKey (Char 'r') Down _ _) game = initialState

-- Ignore 'r' keypress release
handleKeys (EventKey (Char 'r') Up _ _) game = game

-- For an 'k' keypress, kill the player (for testing)
handleKeys (EventKey (Char 'k') Down _ _) game = 
  game { player = damagePlayer 100 (player game) }

-- Ignore 'k' keypress release
handleKeys (EventKey (Char 'k') Up _ _) game = game

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
