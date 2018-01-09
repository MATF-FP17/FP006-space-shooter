module SpriteAnimation
  ( SpriteAnimation (SpriteAnimation)
  , updateAnimation
  , drawAnimation
    --SpriteAnimation (RepeatingAnimation, SingleAnimation)
  ) where

import Graphics.Gloss (Picture(Blank))
  
data AnimationType = Single 
                   | Repeating
                     { aState :: Int
                     , aNumberOfStates :: Int
                     }

data SpriteAnimation = SpriteAnimation
  { aType :: AnimationType
  , aSprites :: [Picture]
  , aChangeTime :: Float
  , aTimeInterval :: Float
  }

updateAnimation :: Float -> SpriteAnimation -> SpriteAnimation  
updateAnimation _ anim@(SpriteAnimation Single [] _ _) = anim
updateAnimation seconds anim@(SpriteAnimation Single sprites time interval) = 
  anim { aSprites = nSs, aChangeTime = nCT}
  where
    nSs :: [Picture]
    nSs = if (time <= 0)
          then tail sprites
          else sprites
    
    nCT :: Float
    nCT = if (time <= 0)
          then time + interval - seconds
          else time - seconds

updateAnimation seconds anim@(SpriteAnimation (Repeating state nos) _ time interval) =
  anim { aType = Repeating nS nos, aChangeTime = nCT }
  where
    nS :: Int
    nS = if (time <= 0)
         then (state + 1) `mod` nos
         else state

    nCT :: Float
    nCT = if (time <= 0)
          then time + interval - seconds
          else time - seconds 


drawAnimation :: SpriteAnimation -> Picture
drawAnimation (SpriteAnimation Single [] _ _) = Blank
drawAnimation (SpriteAnimation Single sprites _ _) = head sprites
drawAnimation (SpriteAnimation (Repeating state _) sprites _ _) = sprites!!state
  
  
{-
--Different implementation

data SpriteAnimation = 
  RepeatingAnimation
  { aSprites :: [Picture]
  , aState :: Int
  , aNumberOfStates :: Int
  , aChangeTime :: Float
  , aTimeInterval :: Float
  } 
  | SingleAnimation
  { aSprites :: [Picture]
  , aChangeTime :: Float
  , aTimeInterval :: Float
  }
 
updateAnimation :: Float -> SpriteAnimation -> SpriteAnimation

updateAnimation seconds anim@(RepeatingAnimation _ state nos time interval) = 
  anim { aState = nS, aChangeTime = nCT }
  where
    nS :: Int
    nS = if (time <= 0)
         then (state + 1) `mod` nos
         else state

    nCT :: Float
    nCT = if (time <= 0)
          then time + interval - seconds
          else time - seconds
    
updateAnimation _ anim@(SingleAnimation [] _ _) = anim -- animation is done  
  
updateAnimation seconds anim@(SingleAnimation sprites time interval) = 
  anim { aSprites = nSs, aChangeTime = nCT}
  where
    nSs :: [Picture]
    nSs = if (time <= 0)
          then tail sprites
          else sprites
    
    nCT :: Float
    nCT = if (time <= 0)
          then time + interval - seconds
          else time - seconds
-}
