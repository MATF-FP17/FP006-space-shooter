module Game
  ( run
  ) where

import Constants
import Player
import Asteroid
import Projectile
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Set hiding (map, show)
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
  ,obstaclesAsteroids :: [Asteroid]          -- list of obstcales
  , playerProjectiles :: [Projectile] -- list of projectile fired
  -- enemyProjectiles :: [Projectile] -- list of projectile fired
  -- ....
  -- timeFromLastAddedEnemy :: Float 
  , keysPressed :: Set Key            -- keeps track of all keys currently held down
  , paused :: Bool                    -- shows if the game is currently paused
  , generator :: StdGen              --seed for random numbers
  } deriving Show                     -- TODO: debug output

-- | The starting state for the game.
initialState :: GameState
initialState = Game
  { player = Player (0,(-150)) 50 0
  -- enemies = []
  -- obstacle = []
  , obstaclesAsteroids = []
  , playerProjectiles = []
  -- enemiesProjectiles = []
  -- timeFromLastAddedEnemy = 0
  , keysPressed = empty
  , paused = False
  ,generator = mkStdGen(23456)
  }

-- DRAW FUNCTIONS --
  
drawInfo :: Picture
drawInfo = 
  Scale textScale textScale $
    color red $
      pictures
      [ rowOrder 1 $ Text "Hello"
      , rowOrder 2 $ Text "and"
      , rowOrder 3 $ Text "welcome"
      ]
  where
    rowOrder :: Float -> Picture -> Picture
    rowOrder number = translate 0 ((-textHeight) * number)

drawControlsInfo :: Picture
drawControlsInfo = 
  Scale controlsTextScale controlsTextScale $ 
    color green $ 
      pictures 
      [ rowROrder 3 $ Text "CONTROLS"
      , rowROrder 2 $ Text "Move: WASD or Arrow Keys" 
      , rowROrder 1 $ Text "Shoot: Space"
      , rowROrder 0 $ Text "Pause: P"
      ]
  where
    rowROrder :: Float -> Picture -> Picture
    rowROrder number = translate 0 (textHeight * number)

drawDebugInfo :: GameState -> Picture
drawDebugInfo game = 
  Scale debugTextScale debugTextScale $ 
    color red $ 
      pictures 
      [ rowROrder 6 $ Text "DEBUG INFO"
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
    
drawPauseScreen :: Picture
drawPauseScreen =
  Scale 0.5 0.5 $
    translate (-250) 0 $
      color yellow $
        Text "PAUSED"
  
-- | Convert a game state into a picture.
render :: GameState  -- ^ The game state to render.
       -> Picture    -- ^ A picture of this game state.
render game =
  pictures 
  [ translate (iWidth /. 2) 0 $ pictures [walls,projectiles,spaceship,reloadBar] -- all game objects
  , translateToInfoSideBar (height /. 2 ) $ drawInfo
  , translateToInfoSideBar 0 $ drawControlsInfo
  , translateToInfoSideBar (-height /. 2 ) $ drawDebugInfo game
  , asteroids
  , if (paused game) then translate (iWidth /. 2) 0 $ drawPauseScreen else Blank
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

-- | Add asteroids to the game
addAsteroidsToGame :: Float -> GameState -> GameState
addAsteroidsToGame seconds game =
    game { obstaclesAsteroids = newObstaclesAsteroids, generator = gen''''' }
    where
    oldObstaclesAsteroids = obstaclesAsteroids game
    gen = generator game
    (x', gen') = randomR ((-width /.2 ) + wallBoundWidth + iWidth /. 2, (width /. 2) - wallBoundWidth + iWidth /. 2) gen :: (Float, StdGen) 
    y'= (height /. 2) - 2.0;
    (step,gen'') = randomR (1,120) gen' ::(Int, StdGen)
    (speedX, gen''') = randomR (lowestAsteroidSpeed, highestAsteroidSpeed) gen'' ::(Float, StdGen)
    (speedY, gen'''') = randomR (lowestAsteroidSpeed,highestAsteroidSpeed) gen''' ::(Float, StdGen)
    (deg, gen''''') = randomR (15,30) gen''' ::(Float, StdGen)
    newObstaclesAsteroids = if (step>118) then (Asteroid (x',y') 32.0 32.0 (speedX,(speedY)) deg imageOfAsteroid) : oldObstaclesAsteroids
                                              else oldObstaclesAsteroids

-- | Player fired a projectile
projectileFiredByPlayer :: GameState -> GameState
projectileFiredByPlayer game = 
  if (  (member (SpecialKey KeySpace) (keysPressed game)) -- is the key for firing a projectile being held
     && (canFireProjectile (player game))  -- has the player reloaded a projectile
     )
  then -- fire
    game { playerProjectiles = 
             addProjectile (px,py') (0,100) (playerProjectiles game) -- FIX: Remove constants for speed from here}
         , player = reload (player game) -- reload after firing  
         }
  else
    game
  where 
    (px,py) = getPlayerPosition (player game)
    py' = py + shipSizeHt

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
update seconds game = if not (paused game) 
                      then 
                        projectileFiredByPlayer .
                        updatePlayerProjectilesInGame seconds . 
                        deleteProjectilesFormGame .
                        updateAsteroidsInGame seconds .
                        addAsteroidsToGame seconds .
                        deleteAsteroidsFromGame .
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
