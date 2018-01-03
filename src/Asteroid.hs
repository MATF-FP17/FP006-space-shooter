module Asteroid
  ( Asteroid (Asteroid)
  , drawAsteroid
  , updateAsteroid
  , addAsteroid
  , asteroidInBounds
  , deleteOutOfBoundsAsteroids
  ) where
  
import Constants
import Graphics.Gloss.Game
import Graphics.Gloss
import Data.Set
import Data.Function

(/.) = (/) `on` fromIntegral -- divides two Integrals as Floats


-- | Data describing all properties of a asteroid
data Asteroid = Asteroid 
     { aPosition :: (Float, Float)  -- asteroid coordinates
     , aHeight :: Float             -- asteroid height =32
     , aWidth :: Float              -- asteroid width =32
     , aSpeed :: (Float, Float)     -- asteroid speed
     , aDegree :: Float             -- rotation degree
     , aPath   :: FilePath            -- image path that presents asteroid
     } deriving Show                
 
 -- | Produces a Picture of a given Asteroid
drawAsteroid :: Asteroid -> Picture
drawAsteroid asteroid =
   translate rx ry $
   Rotate deg $
   png path
     where
       (rx,ry) = aPosition asteroid
       deg = aDegree asteroid
       h = aHeight asteroid
       w = aWidth asteroid
       path = aPath asteroid
  
-- | Update Asteroid
updateAsteroid :: Float -> Asteroid -> Asteroid
updateAsteroid seconds asteroid =
  asteroid { aPosition = (nx', ny'), aDegree=nd }
    where
      (nx,ny) = aPosition asteroid
      (sx,sy) = aSpeed asteroid
      nx' = nx + seconds * sx
      ny' = ny + seconds * sy
      nd= (aDegree asteroid) + 1
  
-- | Adds new asteroid 
addAsteroid :: (Float,Float) ->  Float-> Float -> (Float,Float) -> Float -> FilePath ->[Asteroid] -> [Asteroid]
addAsteroid (px,py) h w (sx,sy) deg path asteroids = (Asteroid (px,py) h w (sx,sy) deg path) : asteroids

-- | Checks if a asteroid has exited the screen
asteroidInBounds :: Asteroid -> Bool
asteroidInBounds asteroid =
  if ( (ry > yLimit   ) || 
       (ry < (-yLimit)) ||
       (rx > xLimitRight) ||
       (rx < xLimitLeft)
     )
  then
    False
  else
    True
  where
    (rx,ry) = aPosition asteroid
    yLimit = height /. 2
    xLimitLeft =(-(width /. 2)) +  iWidth /. 2
    xLimitRight =(width /. 2) + iWidth /. 2

deleteOutOfBoundsAsteroids :: [Asteroid] -> [Asteroid]
deleteOutOfBoundsAsteroids asteroids = Prelude.filter asteroidInBounds asteroids
