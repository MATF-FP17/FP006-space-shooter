module Game
  ( run
  ) where

import Graphics.Gloss
--import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Data.Set

width, height, offset :: Int
width = 400 
height = 600
offset = 100

window :: Display
window = InWindow "SpaceShooter" (width, height) (offset, offset)

background :: Color
background = black

data PlayerState = Player
  { position :: (Float, Float) -- player coordinates
  , speed :: Float             -- player movement speed
  } deriving Show
  

-- | Data describing the state of the game. 
data GameState = Game
  { player :: PlayerState       -- state of the player
  -- enemies :: [Enemy]         -- list of enemies
  -- obstcales :: [Obstacle]    -- list of obstcales
  -- playerRockets :: [Rockets] -- list of rocket fired
  -- enemyRockets :: [Rockets]  -- list of rocket fired
  -- ....
  -- timeFromLastAddedEnemy :: Float 
  , keysPressed :: Set Key      -- keeps track of all keys currently held down
  , paused :: Bool              -- shows if the game is currently paused
  } deriving Show
    
-- | The starting state for the game.
initialState :: GameState
initialState = Game
  { player = Player (0,(-150)) 50
  -- enemies = []
  -- obstacle = []
  -- playerRockets = []
  -- enemiesRockets = []
  -- timeFromLastAddedEnemy = 0
  , keysPressed = empty
  , paused = False
  }
                    



-- | Convert a game state into a picture.
render :: GameState  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [walls,spaceship]
  where
    -- The player
    (px,py) = position $ player game
    
    spaceship :: Picture
    spaceship = 
      translate px py $
        color blue $ 
          polygon [(0,25),(15,(-15)),(0,0),((-15),(-15)),(0,25)]

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate offset 0 $
        color wallColor $
          rectangleSolid 10 580

    wallColor = greyN 0.5
    walls = pictures [wall 185, wall (-185)]
    
    
-- | Update the player
updatePlayer :: Set Key -> Float -> PlayerState -> PlayerState
updatePlayer keysPressed seconds player =
  player { position = (nx', ny') }
    where
      (nx, ny) = position player

      nx' :: Float
      nx' = 
        if (member (Char 'd') keysPressed) 
          || (member (SpecialKey KeyRight) keysPressed)
        then
          (nx + seconds * speed player)
        else if (member (Char 'a') keysPressed)
          || (member (SpecialKey KeyLeft) keysPressed)
        then
          (nx - seconds * speed player)
        else 
          nx
      
      ny' :: Float
      ny' = 
        if (member (Char 'w') keysPressed) 
          || (member (SpecialKey KeyUp) keysPressed)
        then
          (ny + seconds * speed player)
        else if (member (Char 's') keysPressed)
          || (member (SpecialKey KeyDown) keysPressed)
        then
          (ny - seconds * speed player)
        else 
          ny
          
-- | Wrapper: Calls updatePlayer for GameState
updatePlayerInGame :: Float -> GameState -> GameState
updatePlayerInGame seconds game =
  game { player = updatePlayer (keysPressed game) seconds (player game) }


-- | Update the game
update :: Float -> GameState -> GameState 
update seconds game = if not (paused game) 
                      then 
                        updatePlayerInGame seconds game -- TODO: call updates here
                      else 
                        game


-- | Number of frames to show per second.
fps :: Int
fps = 60


-- | Respond to key events.
handleKeys :: Event -> GameState -> GameState

-- For an 'p' keypress, pause the game
handleKeys (EventKey (Char 'p') Down _ _) game =
  game { paused = not (paused game) }

handleKeys (EventKey (Char 'p') Up _ _) game = game

  
handleKeys (EventKey key Down _ _) game =
  game { keysPressed = insert key (keysPressed game) }
  
handleKeys (EventKey key Up _ _) game =
  game { keysPressed = delete key (keysPressed game) }
  

      
-- Do nothing for all other events.
handleKeys _ game = game

run :: IO ()
run = play window background fps initialState render handleKeys update 
