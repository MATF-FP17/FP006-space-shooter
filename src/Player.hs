module Player
  ( PlayerState (Player) 
  , loadPlayerSprites
  , drawSpaceShip
  , drawReloadBar
  , updatePlayer
  , getPlayerPosition
  , canFireProjectile
  , reload
  , noMovement
  , debugPlayerPosition
  , debugPlayerSpeed
  , debugPlayerReloadTime
  --, debugPlayerSpriteState
  --, debugPlayerSpriteChangeTime
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
  , pInReload :: Float          -- time left until a projectile can be fired again
  , pSprites :: [Picture]       -- all sprites of player's spaceship
  --, pSpriteState :: Int         -- index of last drawn sprite
  --, pSpriteChangeTime :: Float  -- time left until sprite change
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
  
{-  
debugPlayerSpriteState :: PlayerState -> String
debugPlayerSpriteState player =  "state " ++ show (pSpriteState player)

debugPlayerSpriteChangeTime :: PlayerState -> String
debugPlayerSpriteChangeTime player =  
  "change " ++ 
    (showFFloat (Just 2) (pSpriteChangeTime player) "" )
-}  
  
-- | Load spaceship sprites
loadPlayerSprites :: [Picture]
loadPlayerSprites = 
  [ png imageShipN0
  , png imageShipN1
  , png imageShipN2
  , png imageShipL0
  , png imageShipL1
  , png imageShipL2
  , png imageShipR0
  , png imageShipR1
  , png imageShipR2
  ]

-- | Produces a Picture of a Player
drawSpaceShip :: PlayerState -> Picture
drawSpaceShip player = 
  translate px py $
    Scale (w/wo) ((hb+ht+hbt)/ho) $ 
      (pSprites player)!!(3*hm+vm) -- picks a sprite based of 9 possible movement states
        --polygon [(0,ht),(w,-hb),(0,0),(-w,-hb),(0,ht)]
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
    
drawReloadBar :: PlayerState -> Picture
drawReloadBar player = 
  translate px py $
    translate 0 (-shipSizeHb - shipSizeHbTail - 6) $
      pictures 
      [ color (greyN 0.5) $ rectangleSolid (shipSizeWh*2) (barHeight)
      , color red $ rectangleSolid (((shipSizeWh*2)-1)*percentage) (barHeight-2)
      ]
  where
    (px,py) = pPosition player
    barHeight = 6
    percentage = (pInReload player) / (playerReloadTime)


canFireProjectile :: PlayerState -> Bool
canFireProjectile player = if (pInReload player <= 0) then True else False
  
reload :: PlayerState -> PlayerState
reload player = player { pInReload = playerReloadTime }

-- | Update the player
updatePlayer :: Set Key -> Float -> PlayerState -> PlayerState
updatePlayer keysPressed seconds player =
  player { pPosition = (nx', ny'), pInReload = nReload,
           -- pSpriteState = nSS', pSpriteChangeTime = nSCT', 
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
      
      {-
      nSS' :: Int
      nSS' =
        if ((pSpriteChangeTime player) <= 0)
        then
          ((pSpriteState player) + 1) `mod` spaceshipSpriteNumber
        else
          pSpriteState player
          
      nSCT' :: Float
      nSCT' = 
        if ((pSpriteChangeTime player) <= 0)
        then
          (pSpriteChangeTime player) + spaceshipSpriteChangeInterval - seconds
        else
          (pSpriteChangeTime player) - seconds
      -}      
        
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

-- | Returns player coordinates
getPlayerPosition :: PlayerState -> (Float, Float)
getPlayerPosition player = pPosition player

-- | Initial state of data Movement
noMovement :: Movement
noMovement = Movement NoH NoV
