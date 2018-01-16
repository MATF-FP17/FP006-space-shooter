module HealthPackage
  ( HealthPackage (HealthPackage)
  , drawHealthPackage
  , updateHealthPackage
  , healthPackageInBounds
  , deleteOutOfBoundsHealthPackage
  , hPosition
  , hWidth
  ) where
  
import Constants
import Graphics.Gloss.Game
import Graphics.Gloss
import Data.Function (on)

(/.) = (/) `on` fromIntegral -- divides two Integrals as Floats

-- | Data describing all properties of a healthBar
data HealthPackage = HealthPackage 
     { hPosition :: (Float, Float)  -- package coordinates
     , hWidth :: Float              -- package width 
     , hSpeed :: (Float, Float)     -- package speed
     , hPicture :: Picture          -- picture object of the asteroid
     } deriving (Show)

 -- | Produces a Picture of a given healt package
drawHealthPackage :: HealthPackage -> Picture
drawHealthPackage hp =
   translate rx ry $
   hPicture hp
     where
       (rx,ry) = hPosition hp

-- | Update health package
updateHealthPackage :: Float -> HealthPackage -> HealthPackage
updateHealthPackage seconds hp =
  hp { hPosition = (nx', ny') }
    where
      (nx,ny) = hPosition hp
      (sx,sy) = hSpeed hp
      nx' = nx + seconds * sx
      ny' = ny + seconds * sy 

-- | Checks if health package has exited the screen
healthPackageInBounds :: HealthPackage -> Bool
healthPackageInBounds hp =
  not ( (ry > yLimit) || 
        (ry < (-yLimit)) ||
        (rx > xLimitRight) ||
        (rx < xLimitLeft) )
  where
    (rx,ry) = hPosition hp
    yLimit = height /. 2
    xLimitLeft =(-width /.2 ) - wallBoundWidth 
    xLimitRight =width /. 2 + wallBoundWidth

-- | Removes all health packages that have exited the screen
deleteOutOfBoundsHealthPackage :: [HealthPackage] -> [HealthPackage]
deleteOutOfBoundsHealthPackage hps = Prelude.filter healthPackageInBounds hps