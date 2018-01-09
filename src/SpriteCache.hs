module SpriteCache
  ( SpriteCache (SpriteCache)
  , loadAllSprites
  , sProjectileSprites
  ) where

import Constants

import Graphics.Gloss
import Graphics.Gloss.Game
--import Graphics.Gloss.Interface.Pure.Game
--import Data.Set hiding (map, show)
--import Data.Function

data SpriteCache = SpriteCache
  { sProjectileSprites :: [Picture]
  } deriving (Show)
  
loadAllSprites :: SpriteCache
loadAllSprites = SpriteCache loadProjectileSpirtes
  
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
  