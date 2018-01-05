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
  } deriving (Show,Eq)               -- TODO: debug output

-- | Produces a Picture of a given Projectile
drawProjectile :: Projectile -> Picture
drawProjectile projectile =
  translate rx ry $
    color yellow $
      circle 4
    where
      (rx,ry) = rPosition projectile

-- | Update a projectile
updateProjectile :: Float -> Projectile -> Projectile
updateProjectile seconds projectile =
  projectile { rPosition = (nx', ny') }
    where
      (nx,ny) = rPosition projectile
      (sx,sy) = rSpeed projectile
      nx' = nx + seconds * sx
      ny' = ny + seconds * sy

-- | Adds new projectile when fired
addProjectile :: (Float,Float) -> (Float,Float) -> [Projectile] -> [Projectile]
addProjectile (px,py) (sx,sy) projectiles = (Projectile (px,py) (sx,sy)) : projectiles

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
