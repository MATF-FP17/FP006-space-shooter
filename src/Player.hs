module Player
  ( PlayerState (Player) 
  , drawSpaceShip
  , drawReloadBar
  , updatePlayer
  , getPlayerPosition
  , canFireProjectile
  , reload
  , debugPlayerPosition
  , debugPlayerSpeed
  , debugPlayerReloadTime
  ) where

import Constants
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Set hiding (show)
import Data.Function
import Numeric

(/.) = (/) `on` fromIntegral -- divides two Integrals as Floats

-- | Data describing all properties of a player
data PlayerState = Player
  { pPosition :: (Float, Float) -- player coordinates
  , pSpeed :: Float             -- player movement speed
  , pInReload :: Float          -- time left until a projectile can be fired again
  } deriving Show               -- TODO: debug output
  
debugPlayerPosition :: PlayerState -> String
debugPlayerPosition player = 
  "position (" ++ 
    (showFFloat (Just 2) px "") ++ 
      "," ++ 
        (showFFloat (Just 2) py "") ++ 
          ")"
  where 
    (px,py) = pPosition player

debugPlayerSpeed :: PlayerState -> String
debugPlayerSpeed player = "speed: " ++ show (pSpeed player)

debugPlayerReloadTime :: PlayerState -> String
debugPlayerReloadTime player = 
  "reload time: " ++
    (showFFloat (Just 4) (pInReload player) "") 

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
    
drawReloadBar :: PlayerState -> Picture
drawReloadBar player = 
  translate px py $
    translate 0 (-shipSizeHb - 7) $
      pictures 
      [
      color (greyN 0.5) $
        rectangleSolid (shipSizeWh*2) (barHeight)
      ,
      color red $
        rectangleSolid (((shipSizeWh*2)-1)*percentage) (barHeight-2)
      ]
  where
    (px,py) = pPosition player
    barHeight = 5
    percentage = (pInReload player) / (playerReloadTime)


canFireProjectile :: PlayerState -> Bool
canFireProjectile player = if (pInReload player <= 0) then True else False
  
reload :: PlayerState -> PlayerState
reload player = player { pInReload = playerReloadTime }

-- | Update the player
updatePlayer :: Set Key -> Float -> PlayerState -> PlayerState
updatePlayer keysPressed seconds player =
  player { pPosition = (nx', ny'), pInReload = nReload }
    where
      (nx, ny) = pPosition player
      w = wallBoundWidth + shipSizeWh
      
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
      
      nReload :: Float
      nReload = max 0 ((pInReload player) - seconds)

-- | Returns player coordinates
getPlayerPosition :: PlayerState -> (Float, Float)
getPlayerPosition player = pPosition player
