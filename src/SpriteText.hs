module SpriteText
  ( makeSpriteText
  , makeSpriteTextTight
  ) where

import Constants
import Data.Map.Strict (Map,(!))
import Data.Char (toUpper)
import Graphics.Gloss
import Graphics.Gloss.Game


makeSpriteText :: [Char] -> (Map Char Picture) -> Picture
makeSpriteText text font =
  translate halfLength halfLength $
    pictures $     
      map trans $  
        zip [0..] $
          map (font!) $
            map toUpper text
  where 
    trans :: (Int,Picture) -> Picture
    trans (index,sprite) = translate (fromIntegral (index * imageSpriteFontSize)) 0 sprite
    halfLength :: Float
    halfLength = fromIntegral imageSpriteFontSize / 2.0
  
makeSpriteTextTight :: [Char] -> (Map Char Picture) -> Picture
makeSpriteTextTight text font =
  translate halfLength halfLength $
    pictures $     
      map trans $  
        zip [0..] $
          map (font!) $
            map toUpper text
  where 
    trans :: (Int,Picture) -> Picture
    trans (index,sprite) = translate (fromIntegral (index * imageSpriteFontSize) * 75.0/100.0) 0 sprite
    halfLength :: Float
    halfLength = fromIntegral imageSpriteFontSize / 2.0