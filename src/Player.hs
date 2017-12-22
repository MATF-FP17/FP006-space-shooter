module Player
  ( PlayerState (Player) 
  , drawSpaceShip
  , updatePlayer
  , getPlayerPosition
  ) where

import Constants
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Set
import Data.Function

(/.) = (/) `on` fromIntegral -- divides two Integrals as Floats

-- | Data describing all properties of a player
data PlayerState = Player
  { pPosition :: (Float, Float) -- player coordinates
  , pSpeed :: Float             -- player movement speed
  } deriving Show               -- TODO: debug output

-- | Produces a Picture of a Player
drawSpaceShip :: PlayerState -> Picture
drawSpaceShip player = 
  translate px py $
    color blue $ 
      polygon [(0,ht),(w,-hb),(0,0),(-w,-hb),(0,ht)]
  where
    (px,py) = pPosition player
    w = shipSizeWh
    ht = shipSizeHt
    hb = shipSizeHb


-- | Update the player
updatePlayer :: Set Key -> Float -> PlayerState -> PlayerState
updatePlayer keysPressed seconds player =
  player { pPosition = (nx', ny') }
    where
      (nx, ny) = pPosition player
      w = wallBoundWidth / 2 + shipSizeWh
      
      nx' :: Float
      nx' = 
        if (member (Char 'd') keysPressed) 
          || (member (SpecialKey KeyRight) keysPressed)
        then
          min (nx + seconds * pSpeed player) (width /. 2 - w)
        else if (member (Char 'a') keysPressed)
          || (member (SpecialKey KeyLeft) keysPressed)
        then
          max (nx - seconds * pSpeed player) ((-width) /. 2 + w)
        else 
          nx

      ny' :: Float
      ny' = 
        if (member (Char 'w') keysPressed) 
          || (member (SpecialKey KeyUp) keysPressed)
        then
          min (ny + seconds * pSpeed player) (height /. 2 - shipSizeHt)
        else if (member (Char 's') keysPressed)
          || (member (SpecialKey KeyDown) keysPressed)
        then
          max (ny - seconds * pSpeed player) (-height /. 2 + shipSizeHb)
        else 
          ny

-- | Returns player coordinates
getPlayerPosition :: PlayerState -> (Float, Float)
getPlayerPosition player = pPosition player
