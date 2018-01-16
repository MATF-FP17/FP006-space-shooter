module SpriteAnimation
  ( SpriteAnimation (SpriteAnimation)
  , updateAnimation
  , drawAnimation
  , makeRepeatingAnimation
  , makeSingleAnimation
  , blankAnimation
    --SpriteAnimation (RepeatingAnimation, SingleAnimation)
  ) where

import Graphics.Gloss (Picture(Blank))
  
-- | Data describing type of animation. 
-- Single animation displays all given sprites only once. After that it can 
-- display only blank picture (Gloss.Picture (Blank))
-- Repeating animation displays given sprites in an infinite loop
data AnimationType = Single    -- draw all sprites only once
                   | Repeating -- draw sprites in infinite loop
                     { aState :: Int          -- current sprite from loop
                     , aNumberOfStates :: Int -- number of different sprites in 
                                              -- aSprites list
                     } deriving (Show)

-- | Data describing an object's animation. Keeps a list of Gloss.Picture 
-- objects which describe all different states of an animation. Keeps track of 
-- which Picture or sprite is to be drawn next and when should the sprite change
-- to next.
data SpriteAnimation = SpriteAnimation
  { aType :: AnimationType    -- type of animation (single or repeating)
  , aSprites :: [Picture]     -- all sprites in the animation
  , aChangeTime :: Float      -- time left until sprite change
  , aTimeInterval :: Float    -- time interval for changing sprites
  } deriving (Show)

-- | Update animation based on time elapsed since last update
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

-- | Gives the current Picture or sprite of the animation
drawAnimation :: SpriteAnimation -> Picture
drawAnimation (SpriteAnimation Single [] _ _) = Blank
drawAnimation (SpriteAnimation Single sprites _ _) = head sprites
drawAnimation (SpriteAnimation (Repeating state _) sprites _ _) = sprites!!state
  
-- | Make a Repeating Animation with given sprite change interval and list of sprites
makeRepeatingAnimation :: Float -> [Picture] -> SpriteAnimation
makeRepeatingAnimation interval sprites = SpriteAnimation (Repeating 0 (length sprites)) sprites interval interval

-- | Make a Single Animation with given sprite change interval and list of sprites
makeSingleAnimation :: Float -> [Picture] -> SpriteAnimation
makeSingleAnimation interval sprites = SpriteAnimation Single sprites interval interval

-- | Make a Blank Animation that only produces a Blank Picture (testing only) --TODO: remove if obsolete
blankAnimation :: SpriteAnimation
blankAnimation = SpriteAnimation Single [] 0 0
