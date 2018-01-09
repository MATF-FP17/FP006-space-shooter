module SpriteCache
  ( SpriteCache (SpriteCache)
  , loadAllSprites
  , loadSpaceshipSprites
  , sProjectileSprites
  , sSpaceshipSprites
  ) where

import Constants

import Graphics.Gloss
import Graphics.Gloss.Game
--import Graphics.Gloss.Interface.Pure.Game
--import Data.Set hiding (map, show)
--import Data.Function

data SpriteCache = SpriteCache
  { sProjectileSprites :: [Picture]
  , sSpaceshipSprites :: [Picture]
  } deriving (Show)
  
loadAllSprites :: SpriteCache
loadAllSprites = SpriteCache loadProjectileSpirtes loadSpaceshipSprites 
  
loadProjectileSpirtes :: [Picture]
loadProjectileSpirtes = 
  [ {-png imageProjectileA1
  , png imageProjectileA2
  , png imageProjectileA3
  , png imageProjectileA4
  , png imageProjectileA5
  , png imageProjectileA6
  , png imageProjectileA7
  , png imageProjectileA8 
  ,-} png imageProjectileB1
  , png imageProjectileB2
  , png imageProjectileB3
  , png imageProjectileB4
  , png imageProjectileB5
  , png imageProjectileB6
  , png imageProjectileB7
  , png imageProjectileB8
  ]
  
loadSpaceshipSprites :: [Picture]
loadSpaceshipSprites =
  [ png imageShipN0
  , png imageShipN1
  , png imageShipN2
  , png imageShipL0
  , png imageShipL1
  , png imageShipL2
  , png imageShipR0
  , png imageShipR1
  , png imageShipR2
  ]
  
  