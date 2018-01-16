module GameDraw
  ( drawGameScreen
  , drawWelcomeScreen
  , drawGameOverScreen
  --, drawInfo
  --, drawControlsInfo
  --, drawDebugInfo
  --, drawPauseScreen
  ) where

import Constants
import GameState --(GameState(Game,WelcomeScreen,GameOver), player, enemies, obstaclesAsteroids, playerProjectiles, enemyProjectiles)
import Player (drawSpaceShip, drawReloadBar, pHealth, pScore, debugPlayerPosition, debugPlayerSpeed, debugPlayerReloadTime)
import Enemy (drawEnemy)
import Asteroid (drawAsteroid)
import HealthPackage (drawHealthPackage)
import Projectile (drawProjectile)
import SpriteText (makeSpriteText, makeSpriteTextTight)
import SpriteCache (Font,sSpriteFont)
import Graphics.Gloss (Picture(..), translate, pictures, color, rectangleSolid)
import Graphics.Gloss.Data.Color
import Data.Map.Strict (Map)

import Data.Function (on)
(/.) = (/) `on` fromIntegral -- divides two Integrals as Floats

-- DRAW FUNCTIONS --
drawGameScreen :: GameState -> Picture
drawGameScreen game = 
  pictures 
  [ translate (iWidth /. 2) 0 $ 
      pictures [walls,projectiles,spaceship,reloadBar,asteroids,allEnemies,eProjectiles,hPackages] -- all game objects
  , translateToInfoSideBar (height /. 2 ) $ 
      drawInfo (sSpriteFont (sprites game)) (pHealth (player game)) (pScore (player game))
  , translateToInfoSideBar 0 $ 
      drawControlsInfo (sSpriteFont (sprites game))
  , if (showDebug game) 
    then translateToInfoSideBar (-height /. 2 ) $ 
           drawDebugInfo game
    else Blank
  , if (paused game) 
    then translate (iWidth /. 2) 0 $ 
           drawPauseScreen (sSpriteFont (sprites game)) 
    else Blank
  ]
  where
    translateToInfoSideBar :: Float -> Picture -> Picture
    translateToInfoSideBar h = translate ((-width /. 2) + (-iWidth /. 2)) h

    -- player's spaceship
    spaceship :: Picture
    spaceship = drawSpaceShip (player game)

    reloadBar :: Picture
    reloadBar = drawReloadBar (player game)

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate offset 0 $
        color (greyN 0.5) $
          rectangleSolid wallBoundWidth (fromIntegral height)

    walls = pictures [ wall ((width /. 2) - (wallBoundWidth / 2))
                     , wall (( (-width) /. 2) + (wallBoundWidth / 2)) ]

    -- Player projectiles
    projectiles :: Picture
    projectiles = pictures $ map drawProjectile (playerProjectiles game) 

    -- Asteroids
    asteroids :: Picture
    asteroids = pictures $ map drawAsteroid (obstaclesAsteroids game)

    -- Enemies
    allEnemies :: Picture
    allEnemies = pictures $ map drawEnemy (enemies game)

    -- Enemy projectiles
    eProjectiles :: Picture
    eProjectiles = pictures $ map drawProjectile (enemyProjectiles game)
    
    -- Health packages
    hPackages :: Picture
    hPackages = pictures $ map drawHealthPackage (healthPackages game)

drawWelcomeScreen :: Font -> Picture
drawWelcomeScreen spriteFont =
  pictures
  [ Scale 2 2 $ rowOrder (-2) $ centerText "SPACE SHOOTER" spriteFont
  , rowOrder 2 $ centerText "Press any key to start" spriteFont
  ]
  where
    rowOrder :: Float -> Picture -> Picture
    rowOrder number = translate 0 (fromIntegral (-imageSpriteFontSize) * number)
    center :: Int -> Picture -> Picture
    center length = translate (-(fromIntegral ((length)*imageSpriteFontSize) / 2.0)) 0
    centerText :: [Char] -> Font -> Picture
    centerText text font = center (length text) $ makeSpriteText text font

drawGameOverScreen :: Font -> Int -> Picture
drawGameOverScreen spriteFont score = 
  pictures
  [ Scale 2 2 $ rowOrder (-2) $ centerText "GAME OVER" spriteFont
  , rowOrder 0 $ centerText ("Score: "++(show score)) spriteFont
  , rowOrder 2 $ centerText "Press R to restart" spriteFont
  ]
  where
    rowOrder :: Float -> Picture -> Picture
    rowOrder number = translate 0 (fromIntegral (-imageSpriteFontSize) * number)
    center :: Int -> Picture -> Picture
    center length = translate (-(fromIntegral ((length)*imageSpriteFontSize) / 2.0)) 0
    centerText :: [Char] -> Font -> Picture
    centerText text font = center (length text) $ makeSpriteText text font 



drawInfo :: Font -> Int -> Int -> Picture
drawInfo spriteFont health score = 
  pictures
  [ rowOrder 1 $ makeSpriteText ("Health:"++(show health)) spriteFont
  , rowOrder 2 $ makeSpriteText ("Score:"++(show score)) spriteFont
  ]
  where
    rowOrder :: Float -> Picture -> Picture
    rowOrder number = translate 0 (fromIntegral (-imageSpriteFontSize) * number)

drawControlsInfo :: Font -> Picture
drawControlsInfo spriteFont = 
  --Scale 0.8 0.8 $ 
  pictures
  [ rowROrder 5 $ makeSpriteTextTight "CONTROLS" spriteFont
  , rowROrder 4 $ makeSpriteTextTight "Move :WASD" spriteFont
  , rowROrder 3 $ makeSpriteTextTight "Shoot:Space" spriteFont
  , rowROrder 2 $ makeSpriteTextTight "Pause:P" spriteFont
  , rowROrder 1 $ makeSpriteTextTight "Reset:R" spriteFont
  , rowROrder 0 $ makeSpriteTextTight "Debug:O" spriteFont
  ]
  where
    rowROrder :: Float -> Picture -> Picture
    rowROrder number = translate 0 (fromIntegral (imageSpriteFontSize) * number)

drawDebugInfo :: GameState -> Picture
drawDebugInfo game =
  Scale debugTextScale debugTextScale $ 
    color red $ 
      pictures 
      [ rowROrder 8 $ Text "DEBUG INFO"
      , rowROrder 7 $ Text $ "No. of enemies: " ++ 
                             show (length (enemies game))
      , rowROrder 6 $ Text $ "No. of asteroids: " ++ 
                             show (length (obstaclesAsteroids game))
      , rowROrder 5 $ Text $ "No. of player projectiles: " ++ 
                             show (length (playerProjectiles game))
      , rowROrder 4 $ Text $ "No. of enemy projectiles: " ++ 
                             show (length (enemyProjectiles game))
      , rowROrder 3 $ Text "Player:"
      , rowROrder 2 $ Text $ debugPlayerPosition (player game) 
      , rowROrder 1 $ Text $ debugPlayerReloadTime (player game)
      ]
  where
    rowROrder :: Float -> Picture -> Picture
    rowROrder number = translate 0 (textHeight * number)

drawPauseScreen :: Font -> Picture
drawPauseScreen spriteFont =
  centerText "PAUSED" spriteFont
  where
    center :: Int -> Picture -> Picture
    center length = translate (-(fromIntegral ((length)*imageSpriteFontSize) / 2.0)) 0
    centerText :: [Char] -> Font -> Picture
    centerText text font = center (length text) $ makeSpriteText text font

