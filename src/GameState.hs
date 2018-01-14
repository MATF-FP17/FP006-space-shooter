module GameState where

import Constants
import SpriteCache (SpriteCache, loadAllSprites, sSpaceshipSprites)
import Player (PlayerState(Player), noMovement)
import Asteroid (Asteroid)
import Projectile (Projectile)
import Enemy (Enemy)
import Graphics.Gloss.Interface.Pure.Game (Key)
import Data.Set (Set, empty)
import System.Random (StdGen, mkStdGen)

-- | Data describing the state of the game. 
data GameState = Game                 -- Game in progress
  { player :: PlayerState             -- state of the player
  , enemies :: [Enemy]                -- list of enemies
  , obstaclesAsteroids :: [Asteroid]  -- list of obstcales
  , playerProjectiles :: [Projectile] -- list of projectiles fired by the player
  , enemyProjectiles :: [Projectile]  -- list of projectiles fired by the enemies
  , timeToAddNewEnemy :: Float        -- time left until a new enemy should be added
  , keysPressed :: Set Key            -- keeps track of all keys currently held down
  , paused :: Bool                    -- shows if the game is currently paused
  , generator :: StdGen               -- seed for random numbers
  , sprites :: SpriteCache            -- cache with all game spirtes
  }
  | WelcomeScreen                     -- Program's strating screen
  { keysPressed :: Set Key
  , sprites :: SpriteCache
  } 
  | GameOver                          -- Screen show after player's death
  { keysPressed :: Set Key
  , sprites :: SpriteCache
  , score :: Int                      -- player's score from game
  }

-- | The starting state for the game.
initialState :: GameState
initialState = WelcomeScreen
  { keysPressed = empty
  , sprites = loadAllSprites
  }

-- | Transfering loaded sprites into GameState
initialLoadedGameState :: SpriteCache -> GameState
initialLoadedGameState loadedSprites = Game  
  { player = Player (0,(-150)) 100 100 0 0 (sSpaceshipSprites loadedSprites) noMovement
  , enemies = []
  , obstaclesAsteroids = []
  , playerProjectiles = []
  , enemyProjectiles = []
  , timeToAddNewEnemy = enemySpawnTime
  , keysPressed = empty
  , paused = False
  , generator = mkStdGen(23456)
  , sprites = loadedSprites
  }
