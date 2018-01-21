module SpriteText
  ( makeSpriteText
  , makeSpriteTextTight
  ) where

import Constants
import Data.Map.Strict ((!))
import Data.Char (toUpper)
import Graphics.Gloss
import SpriteCache (Font) --type Font = Map Char Picture

makeSpriteText :: [Char] -> Font -> Picture
makeSpriteText txt font =
  translate halfLength halfLength $
    pictures $     
      map trans $  
        zip [0..] $
          map (font!) $
            map toUpper txt
  where 
    trans :: (Int,Picture) -> Picture
    trans (index,sprite) = translate (fromIntegral (index * imageSpriteFontSize)) 0 sprite
    halfLength :: Float
    halfLength = fromIntegral imageSpriteFontSize / 2.0
  
makeSpriteTextTight :: [Char] -> Font -> Picture
makeSpriteTextTight txt font =
  translate halfLength halfLength $
    pictures $     
      map trans $  
        zip [0..] $
          map (font!) $
            map toUpper txt
  where 
    trans :: (Int,Picture) -> Picture
    trans (index,sprite) = translate (fromIntegral (index * imageSpriteFontSize) * 75.0/100.0) 0 sprite
    halfLength :: Float
    halfLength = fromIntegral imageSpriteFontSize / 2.0