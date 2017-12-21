module Game
  ( run
  ) where

import Graphics.Gloss
--import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game


width, height, offset :: Int
width = 400 
height = 600
offset = 100

window :: Display
window = InWindow "SpaceShooter" (width, height) (offset, offset)

background :: Color
background = black


-- | Data describing the state of the game. 
data GameState = Game
  { player :: (Float, Float)   -- player coordinates
  -- enemies :: [Enemy]         -- list of enemies
  -- obstcales :: [Obstacle]    -- list of obstcales
  -- playerRockets :: [Rockets] -- list of rocket fired
  -- enemyRockets :: [Rockets]  -- list of rocket fired
  -- ....
  -- timeFromLastAddedEnemy :: Float 
  , paused :: Bool
  } deriving Show
    
-- | The starting state for the game.
initialState :: GameState
initialState = Game
  { player = (0,(-150))
  -- enemies = []
  -- obstacle = []
  -- playerRockets = []
  -- enemiesRockets = []
  -- timeFromLastAddedEnemy = 0
  , paused = False
  }
                    



-- | Convert a game state into a picture.
render :: GameState  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [walls,spaceship]
  where
    -- The player
    (px,py) = player game
    
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
    


-- | Update the game
update :: Float -> GameState -> GameState 
update seconds game = if not (paused game) 
                      then 
                        game -- TODO: call updates here
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
   
-- Do nothing for all other events.
handleKeys _ game = game

run :: IO ()
run = play window background fps initialState render handleKeys update 
