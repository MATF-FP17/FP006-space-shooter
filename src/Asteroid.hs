module Asteroid
  ( Asteroid (Asteroid)
  , drawAsteroid
  , updateAsteroid
  , asteroidInBounds
  , deleteOutOfBoundsAsteroids
  , aPosition
  , aWidth
  ) where
  
import Constants
import Graphics.Gloss.Game
import Graphics.Gloss
import Data.Function (on)

(/.) :: Int -> Int -> Float
(/.) = (/) `on` fromIntegral -- divides two Integrals as Floats

-- | Data describing all properties of a asteroid - circle
data Asteroid = Asteroid 
     { aPosition :: (Float, Float)  -- asteroid coordinates
     , aWidth :: Float              -- asteroid width , radus
     , aSpeed :: (Float, Float)     -- asteroid speed
     , aDegree :: Float             -- rotation degree
     , aPicture :: Picture          -- picture object of the asteroid
     } deriving (Show)           

-- | Equality only checks coordinates, speed and rotation degree
instance Eq Asteroid where
  (Asteroid a1p a1w a1s a1d _) == (Asteroid a2p a2w a2s a2d _) = (a1p==a2p && a1w==a2w && a1s==a2s && a1d==a2d )
  (Asteroid a1p a1w a1s a1d _) /= (Asteroid a2p a2w a2s a2d _) = (a1p/=a2p || a1w==a2w || a1s/=a2s || a1d/=a2d )
 
 -- | Produces a Picture of a given Asteroid
drawAsteroid :: Asteroid -> Picture
drawAsteroid asteroid =
   translate rx ry $
   Rotate deg $
   aPicture asteroid
     where
       (rx,ry) = aPosition asteroid
       deg = aDegree asteroid
  
-- | Update Asteroid
updateAsteroid :: Float -> Asteroid -> Asteroid
updateAsteroid seconds asteroid =
  asteroid { aPosition = (nx', ny'), aSpeed = (sx' , sy), aDegree=nd }
    where
      (nx,ny) = aPosition asteroid
      (sx,sy) = aSpeed asteroid
      awidth = aWidth asteroid
      sx' = if (((nx + awidth / 2)> width /. 2 - wallBoundWidth) || ((nx - awidth / 2)< (-width /.2 ) + wallBoundWidth )) then (-sx) else sx 
      nx' = nx + seconds * sx'
      ny' = ny + seconds * sy
      nd= (aDegree asteroid) + 1

-- | Checks if a asteroid has exited the screen
asteroidInBounds :: Asteroid -> Bool
asteroidInBounds asteroid =
  not ( (ry > yLimit) || 
        (ry < (-yLimit)) ||
        (rx > xLimitRight) ||
        (rx < xLimitLeft) )
  where
    (rx,ry) = aPosition asteroid
    yLimit = height /. 2
    xLimitLeft =(-width /.2 ) - wallBoundWidth 
    xLimitRight =width /. 2 + wallBoundWidth

-- | Removes all asteroids that have exited the screen
deleteOutOfBoundsAsteroids :: [Asteroid] -> [Asteroid]
deleteOutOfBoundsAsteroids asteroids = Prelude.filter asteroidInBounds asteroids
