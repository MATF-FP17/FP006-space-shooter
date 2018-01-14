module SpriteCache
  ( SpriteCache (SpriteCache)
  , loadAllSprites
  , sProjectileSprites
  , sSpaceshipSprites
  , sEnemySprites
  , sAsteroidSpriteSmall
  , sAsteroidSpriteBig
  , sSpriteFont
  ) where

import Constants
import Data.Map.Strict (Map, fromList)
import Graphics.Gloss
import Graphics.Gloss.Game

data SpriteCache = SpriteCache
  { sProjectileSprites :: [[Picture]]
  , sSpaceshipSprites :: [Picture]
  , sEnemySprites :: [Picture]
  , sAsteroidSpriteSmall :: Picture
  , sAsteroidSpriteBig :: Picture
  , sSpriteFont :: Map Char Picture  
  } deriving (Show)
  
loadAllSprites :: SpriteCache
loadAllSprites = SpriteCache loadProjectileSpirtes 
                             loadSpaceshipSprites 
                             loadEnemySprites 
                             loadAsteroidSpriteSmall 
                             loadAsteroidSpriteBig
                             loadSpriteFont
  
loadProjectileSpirtes :: [[Picture]]
loadProjectileSpirtes = 
  [ [ png imageProjectileA1
    , png imageProjectileA2
    , png imageProjectileA3
    , png imageProjectileA4
    , png imageProjectileA5
    , png imageProjectileA6
    , png imageProjectileA7
    , png imageProjectileA8
    ] 
  , [ png imageProjectileB1
    , png imageProjectileB2
    , png imageProjectileB3
    , png imageProjectileB4
    , png imageProjectileB5
    , png imageProjectileB6
    , png imageProjectileB7
    , png imageProjectileB8 
    ]
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
  
loadEnemySprites :: [Picture]
loadEnemySprites =
  [ png imageEnemyNormal
  , png imageEnemyLeft 
  , png imageEnemyRight
  ]
  
loadAsteroidSpriteSmall :: Picture
loadAsteroidSpriteSmall = png imageOfAsteroidSmall
  
loadAsteroidSpriteBig :: Picture
loadAsteroidSpriteBig = png imageOfAsteroidBig
 
loadSpriteFont :: Map Char Picture  
loadSpriteFont = fromList loadSpriteFont'
  
loadSpriteFont' :: [(Char,Picture)]
loadSpriteFont' =
  [ ('0', png imageSpriteFont0)
  , ('1', png imageSpriteFont1)
  , ('2', png imageSpriteFont2)
  , ('3', png imageSpriteFont3)
  , ('4', png imageSpriteFont4)
  , ('5', png imageSpriteFont5)
  , ('6', png imageSpriteFont6)
  , ('7', png imageSpriteFont7)
  , ('8', png imageSpriteFont8)
  , ('9', png imageSpriteFont9)
  , ('A', png imageSpriteFontA)
  , ('B', png imageSpriteFontB)
  , ('C', png imageSpriteFontC)
  , ('D', png imageSpriteFontD)
  , ('E', png imageSpriteFontE)
  , ('F', png imageSpriteFontF)
  , ('G', png imageSpriteFontG)
  , ('H', png imageSpriteFontH)
  , ('I', png imageSpriteFontI)
  , ('J', png imageSpriteFontJ)
  , ('K', png imageSpriteFontK)
  , ('L', png imageSpriteFontL)
  , ('M', png imageSpriteFontM)
  , ('N', png imageSpriteFontN)
  , ('O', png imageSpriteFontO)
  , ('P', png imageSpriteFontP)
  , ('Q', png imageSpriteFontQ)
  , ('R', png imageSpriteFontR)
  , ('S', png imageSpriteFontS)
  , ('T', png imageSpriteFontT)
  , ('U', png imageSpriteFontU)
  , ('V', png imageSpriteFontV)
  , ('W', png imageSpriteFontW)
  , ('X', png imageSpriteFontX)
  , ('Y', png imageSpriteFontY)
  , ('Z', png imageSpriteFontZ)
  , (',', png imageSpriteFontComma)
  , ('.', png imageSpriteFontDot)
  , (';', png imageSpriteFontDotComma)
  , (':', png imageSpriteFontDoubleDot)
  , ('=', png imageSpriteFontEqual)
  , ('!', png imageSpriteFontExclamation)
  , ('-', png imageSpriteFontMinus)
  , ('+', png imageSpriteFontPlus)
  , ('?', png imageSpriteFontQuestion)
  , (' ', png imageSpriteFontSpace)
  ]

  
  