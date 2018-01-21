module Enemy
  ( Enemy (Enemy) 
  , initialEnemyState
  , ePosition
  , eShape
  , drawEnemy
  , updateEnemy
  , canEnemyFireProjectile
  , reloadEnemy
  , deleteOutOfBoundsEnemies
  ) where

import Constants
import Graphics.Gloss
import Data.Function (on)
import ObjectCollision (Poly, polyFrom, translatePoly, scalePoly)

(/.) :: Int -> Int -> Float
(/.) = (/) `on` fromIntegral -- divides two Integrals as Floats

-- | Data describing all properties of an enemy
data Enemy = Enemy
  { ePosition :: (Float, Float) -- enemy coordinates
  , eSpeed :: (Float, Float)    -- enemy movement speed
  , eInReload :: Float          -- time left until a projectile can be fired again
  --, eHealth :: Int              -- enemy's health (spaceship's hull integrity)
  , eSprites :: [Picture]       -- all sprites of enemy's spaceship
  , eShape :: Poly Float        -- shape of enemy's spaceship (used in collision)
  } deriving Show
  
initialEnemyState :: (Float,Float) -> (Float,Float) -> [Picture] -> Enemy
initialEnemyState (ex,ey) (sx,sy) sprites = Enemy (ex,ey) (sx,sy) enemyInitialReloadTime sprites shape
  where    
    ew0 = fromIntegral imageEnemyWidth
    eh0 = fromIntegral imageEnemyHeight
    shape :: Poly Float
    shape = polyFrom $ translatePoly ex ey $ scalePoly (enemySizeW/ew0) (enemySizeH/eh0) $ enemyObject
  
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
  enemy { ePosition = (nx', ny') 
        , eInReload = nReload 
        , eShape = nShape
        }
  where
    (nx,ny) = ePosition enemy
    (sx,sy) = eSpeed enemy
    nx' = nx + seconds * sx
    ny' = ny + seconds * sy 
    nReload :: Float
    nReload = max 0 ((eInReload enemy) - seconds)
    ew0 = fromIntegral imageEnemyWidth
    eh0 = fromIntegral imageEnemyHeight
    nShape :: Poly Float
    nShape = polyFrom $ translatePoly nx' ny' $ scalePoly (enemySizeW/ew0) (enemySizeH/eh0) $ enemyObject
    
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

-- | Checks if the enemy is not reloading at the current time
canEnemyFireProjectile :: Enemy -> Bool
canEnemyFireProjectile enemy = (eInReload enemy <= 0)
  
-- | Resets reload timer. Called after a projectile is fired
reloadEnemy :: Enemy -> Enemy
reloadEnemy enemy = enemy { eInReload = enemyReloadTime }


