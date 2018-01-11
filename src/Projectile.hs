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
import Data.Function (on)
import SpriteAnimation

(/.) = (/) `on` fromIntegral -- divides two Integrals as Floats

-- | Data describing all properties of a projectile
data Projectile = Projectile
  { rPosition :: (Float, Float) -- projectile coordinates
  , rSpeed :: (Float, Float)    -- speed vector
  , rAnimation :: SpriteAnimation 
  } deriving (Show)           -- TODO: debug output
  
-- | Equality only checks if the coordinates (rPosition) is the same
instance Eq Projectile where
  (Projectile p1 _ _ ) == (Projectile p2 _ _ ) = ( p1==p2 ) 
  (Projectile p1 _ _ ) /= (Projectile p2 _ _ ) = ( p1/=p2 )
 
 
-- | Produces a Picture of a given Projectile
drawProjectile :: Projectile -> Picture
drawProjectile projectile =
  translate rx ry $
    drawAnimation (rAnimation projectile)
    --(rSprites projectile)!!(rSpriteState projectile)
    --color yellow $
      --circle 4
    where
      (rx,ry) = rPosition projectile

-- | Update a projectile
updateProjectile :: Float -> Projectile -> Projectile
updateProjectile seconds projectile =
  projectile { rPosition = (nx', ny')
             , rAnimation = updateAnimation seconds (rAnimation projectile)
             }
    where
      (nx,ny) = rPosition projectile
      (sx,sy) = rSpeed projectile
      nx' = nx + seconds * sx
      ny' = ny + seconds * sy

-- | Adds new projectile when fired
addProjectile :: (Float,Float) -> (Float,Float) -> SpriteAnimation -> [Projectile] -> [Projectile]
addProjectile (px,py) (sx,sy) animation projectiles = (Projectile (px,py) (sx,sy) animation) : projectiles

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

-- | Removes all projectiles that have exited the screen
deleteOutOfBoundsProjectiles :: [Projectile] -> [Projectile]
deleteOutOfBoundsProjectiles projectiles = filter projectileInBounds projectiles
