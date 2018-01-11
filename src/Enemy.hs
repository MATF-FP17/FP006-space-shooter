module Enemy
  ( Enemy (Enemy) 
  , ePosition
  , drawEnemy
  , updateEnemy
  , deleteOutOfBoundsEnemies
  ) where

import Constants
import Graphics.Gloss
import Graphics.Gloss.Game
import Graphics.Gloss.Interface.Pure.Game
--import Data.Set (Set, member)
import Data.Function (on)
--import Numeric (showFFloat)

(/.) = (/) `on` fromIntegral -- divides two Integrals as Floats

-- | Data describing all properties of an enemy
data Enemy = Enemy
  { ePosition :: (Float, Float) -- enemy coordinates
  , eSpeed :: (Float, Float)    -- enemy movement speed
  --, eHealth :: Int              -- enemy's health (spaceship's hull integrity)
  , eSprites :: [Picture]       -- all sprites of enemy's spaceship
  } deriving Show
  
drawEnemy :: Enemy -> Picture
drawEnemy enemy =
  translate ex ey $
    Scale (enemySizeW/iew) (enemySizeH/ieh) $
      head (eSprites enemy) -- TODO: Draw different sprite based on movement
  where
    (ex,ey) = ePosition enemy
    iew = fromIntegral imageEnemyWidth
    ieh = fromIntegral imageEnemyHeight
 
updateEnemy :: Float -> Enemy -> Enemy
updateEnemy seconds enemy = 
  enemy { ePosition = (nx', ny') }
  where
    (nx,ny) = ePosition enemy
    (sx,sy) = eSpeed enemy
    nx' = nx + seconds * sx
    ny' = ny + seconds * sy 
    
-- | Checks if an Enemy has exited the screen
enemyInBounds :: Enemy -> Bool
enemyInBounds enemy =
  not ( (ey > yLimit   ) || 
        (ey < (-yLimit)) ||
        (ex > xLimit   ) ||
        (ex < (-xLimit)) )
  where
    (ex,ey) = ePosition enemy
    yLimit = height /. 2
    xLimit = width /. 2

-- | Removes all enemies that have exited the screen
deleteOutOfBoundsEnemies :: [Enemy] -> [Enemy]
deleteOutOfBoundsEnemies enemies = filter enemyInBounds enemies
