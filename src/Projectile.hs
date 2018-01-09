module Projectile
  ( Projectile (Projectile)
  , drawProjectile
  , updateProjectile
  , addProjectile
  , projectileInBounds
  , deleteOutOfBoundsProjectiles
  , rPosition
  ) where

import Constants
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Set hiding (filter)
import Data.Function

(/.) = (/) `on` fromIntegral -- divides two Integrals as Floats

-- | Data describing all properties of a projectile
data Projectile = Projectile
  { rPosition :: (Float, Float) -- projectile coordinates
  , rSpeed :: (Float, Float)    -- speed vector
  , rSprites :: [Picture]       -- all projectile sprites
  , rSpriteState :: Int         -- index of last drawn sprite
  , rSpriteChangeTime :: Float  -- time left until sprite change
  } deriving (Show,Eq)          -- TODO: debug output

-- | Produces a Picture of a given Projectile
drawProjectile :: Projectile -> Picture
drawProjectile projectile =
  translate rx ry $
    (rSprites projectile)!!(rSpriteState projectile)
    --color yellow $
      --circle 4
    where
      (rx,ry) = rPosition projectile

-- | Update a projectile
updateProjectile :: Float -> Projectile -> Projectile
updateProjectile seconds projectile =
  projectile { rPosition = (nx', ny')
             , rSpriteState = nSS'
             , rSpriteChangeTime = nSCT'
             }
    where
      (nx,ny) = rPosition projectile
      (sx,sy) = rSpeed projectile
      nx' = nx + seconds * sx
      ny' = ny + seconds * sy

      nSS' :: Int
      nSS' =
        if ((rSpriteChangeTime projectile) <= 0)
        then
          ((rSpriteState projectile) + 1) `mod` projectileSpriteNumber
        else
          rSpriteState projectile

      nSCT' :: Float
      nSCT' = 
        if ((rSpriteChangeTime projectile) <= 0)
        then
          (rSpriteChangeTime projectile) + projectileSpriteChangeInterval - seconds
        else
          (rSpriteChangeTime projectile) - seconds


-- | Adds new projectile when fired
addProjectile :: (Float,Float) -> (Float,Float) -> [Picture] -> [Projectile] -> [Projectile]
addProjectile (px,py) (sx,sy) sprites projectiles = (Projectile (px,py) (sx,sy)) sprites 0 0.1 : projectiles

-- | Checks if a Projectile has exited the screen
projectileInBounds :: Projectile -> Bool
projectileInBounds projectile =
  if ( (ry > yLimit   ) || 
       (ry < (-yLimit)) ||
       (rx > xLimit   ) ||
       (rx < (-xLimit))
     )
  then
    False
  else
    True
  where
    (rx,ry) = rPosition projectile
    yLimit = height /. 2
    xLimit = width /. 2

deleteOutOfBoundsProjectiles :: [Projectile] -> [Projectile]
deleteOutOfBoundsProjectiles projectiles = filter projectileInBounds projectiles
