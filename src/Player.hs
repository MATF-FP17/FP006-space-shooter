module Player
  ( PlayerState (Player) 
  , drawSpaceShip
  , drawReloadBar
  , updatePlayer
  , pPosition
  , pHealth
  , damagePlayer
  , canFireProjectile
  , reload
  , noMovement
  , debugPlayerPosition
  , debugPlayerSpeed
  , debugPlayerReloadTime
  ) where

import Constants
import Graphics.Gloss
import Graphics.Gloss.Game
import Graphics.Gloss.Interface.Pure.Game
import Data.Set hiding (show)
import Data.Function
import Numeric

(/.) = (/) `on` fromIntegral -- divides two Integrals as Floats

-- | Data for describing direction of horizontal movement
data HorizontalMovement = NoH | LeftH | RightH
  deriving (Eq, Enum, Show)
  
-- | Numerating different horizontal directions 
-- (this numeration matches the sprite loading order)
toNumberHorizontalMovement :: HorizontalMovement -> Int
toNumberHorizontalMovement enum = case enum of
  NoH -> 0
  LeftH -> 1
  RightH -> 2
  
-- | Data for describing direction of vertical movement
data VerticalMovement = NoV | UpV | DownV
  deriving (Eq, Enum, Show)
  
-- | Numerating different vertical directions
-- (this numeration matches the sprite loading order)
toNumberVerticalMovement :: VerticalMovement -> Int
toNumberVerticalMovement enum = case enum of
  NoV -> 0
  UpV -> 1
  DownV -> 2
  
-- | Data combining both horizontal and vertical movement
-- Describes 9 possible states of movement (8 directions + 1 for no movement)
data Movement = Movement
  { mHorizontal :: HorizontalMovement
  , mVertical :: VerticalMovement
  } deriving Show
  
-- | Data describing all properties of a player
data PlayerState = Player
  { pPosition :: (Float, Float) -- player coordinates
  , pSpeed :: Float             -- player movement speed
  , pHealth :: Int              -- player's health (spaceship's hull integrity)
  , pInReload :: Float          -- time left until a projectile can be fired again
  , pSprites :: [Picture]       -- all sprites of player's spaceship
  , pMovement :: Movement       -- direction of last movement
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
    Scale (w/wo) ((hb+ht+hbt)/ho) $ 
      (pSprites player)!!(3*hm+vm) -- picks a sprite based of 9 possible movement states
  where
    (px,py) = pPosition player
    w = shipSizeWh * 2
    ht = shipSizeHt
    hb = shipSizeHb
    hbt = shipSizeHbTail
    wo = fromIntegral imageShipWidth
    ho = fromIntegral imageShipHeight
    hm = toNumberHorizontalMovement (mHorizontal (pMovement player))
    vm = toNumberVerticalMovement (mVertical (pMovement player))
    
-- | Draws a reload bar below the player giving visual representation of how much
-- time left there is until another projectile can be fired
drawReloadBar :: PlayerState -> Picture
drawReloadBar player = 
  translate px py $
    translate 0 (-shipSizeHb - shipSizeHbTail - barDistance) $
      pictures 
      [ color (greyN 0.5) $ rectangleSolid (shipSizeWh*2) (barHeight)
      , color red $ rectangleSolid (((shipSizeWh*2)-1)*percentage) (barHeight-2)
      ]
  where
    (px,py) = pPosition player
    barHeight = 6
    barDistance = 6
    percentage = (pInReload player) / (playerReloadTime)

-- | Checks if the player is not reloading at the current time
canFireProjectile :: PlayerState -> Bool
canFireProjectile player = if (pInReload player <= 0) then True else False
  
-- | Resets reload timer. Called after a projectile is fired
reload :: PlayerState -> PlayerState
reload player = player { pInReload = playerReloadTime }

-- | Update the player
updatePlayer :: Set Key -> Float -> PlayerState -> PlayerState
updatePlayer keysPressed seconds player =
  player { pPosition = (nx', ny'), pInReload = nReload,
           pMovement = Movement nH' nV'
         }
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
        
      nH' :: HorizontalMovement
      nH' = 
        if (member (Char 'd') keysPressed) 
          || (member (SpecialKey KeyRight) keysPressed)
        then RightH
        else if (member (Char 'a') keysPressed)
          || (member (SpecialKey KeyLeft) keysPressed)
        then LeftH
        else NoH  
          
      nV' :: VerticalMovement
      nV' = 
        if (member (Char 'w') keysPressed) 
          || (member (SpecialKey KeyUp) keysPressed)
        then UpV
        else if (member (Char 's') keysPressed)
          || (member (SpecialKey KeyDown) keysPressed)
        then DownV
        else NoV

damagePlayer :: Int -> PlayerState -> PlayerState
damagePlayer damage player = player { pHealth = (pHealth player) - damage }
        
-- | Initial state of data Movement
noMovement :: Movement
noMovement = Movement NoH NoV
