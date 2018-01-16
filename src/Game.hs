module Game
  ( run
  ) where

import Constants
import GameState 
import GameDraw
import GameCollision
import GameUpdate
import SpriteCache (sProjectileSprites, sEnemySprites, sAsteroidSpriteSmall, sAsteroidSpriteBig, sSpriteFont, sHealthImproveSprites)
import Player (Player, updatePlayer, canPlayerFireProjectile, reloadPlayer, pScore, pPosition, pHealth)
import Graphics.Gloss.Interface.Pure.Game
import Data.Set (insert, delete, member)
import Data.Function (on)

window :: Display
window = InWindow "SpaceShooter" (width + iWidth, height) (offset, offset)

background :: Color
background = black

-- | Convert a game state into a picture.
render :: GameState  -- ^ The game state to render.
       -> Picture    -- ^ A picture of this game state.
render (WelcomeScreen _ sprites) = drawWelcomeScreen (sSpriteFont sprites)
render (GameOver _ sprites score) = drawGameOverScreen (sSpriteFont sprites) score
render game = drawGameScreen game

-- | Update the game
update :: Float -> GameState -> GameState 
update _ game@(WelcomeScreen _ _) = updateWelcomeScreen game
update _ game@(GameOver _ _ _) = updateGameOverScreen game
update seconds game = if (pHealth (player game)) <= 0
                      then (GameOver (keysPressed game) (sprites game) (pScore (player game)))
                      else if not (paused game) 
                      then 
                        handleInputGameScreen . --must be last
                        addEnemiesToGame seconds .
                        addAsteroidsToGame seconds .
                        addHealthPackagesToGame seconds .
                        addProjectilesFiredByEnemies .
                        addProjectileFiredByPlayer .
                        handlePlayerProjectilesCollision .
                        handleEnemiesProjectilesCollision .
                        handlePlayerAsteroidsCollision .
                        handleProjectilesAsteroidsCollision .
                        handlePlayerHealthPackageCollision .
                        deleteObjectsFromGame .
                        updateObjectsInGame seconds $ 
                        game
                      else 
                        handleInputGameScreen game


-- INPUT FUNCTIONS --

-- | Respond to key events.
handleKeys :: Event -> GameState -> GameState

-- Remember all keys being pressed
handleKeys (EventKey key Down _ _) game =
  game { keysPressed = insert key (keysPressed game) }

-- Forget all keys not pressed anymore
handleKeys (EventKey key Up _ _) game =
  game { keysPressed = delete key (keysPressed game) }

-- Do nothing for all other events.
handleKeys _ game = game


run :: IO ()
run = play window background fps initialState render handleKeys update 
