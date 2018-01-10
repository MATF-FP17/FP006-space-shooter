module Game
  ( run
  ) where

import Constants
import SpriteCache
import Player
import Asteroid
import Projectile
import SpriteAnimation
import SpriteText
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Set hiding (map, show)
import Data.Map.Strict (Map)
import Data.Function
import System.Random

(/.) = (/) `on` fromIntegral -- divides two Integrals as Floats

window :: Display
window = InWindow "SpaceShooter" (width + iWidth, height) (offset, offset)

background :: Color
background = black

-- | Data describing the state of the game. 
data GameState = Game
  { player :: PlayerState             -- state of the player
  -- enemies :: [Enemy]               -- list of enemies
  , obstaclesAsteroids :: [Asteroid]  -- list of obstcales
  , playerProjectiles :: [Projectile] -- list of projectiles fired
  -- enemyProjectiles :: [Projectile] -- list of projectiles fired
  -- ....
  -- timeFromLastAddedEnemy :: Float 
  , keysPressed :: Set Key            -- keeps track of all keys currently held down
  , paused :: Bool                    -- shows if the game is currently paused
  , generator :: StdGen               -- seed for random numbers
  , sprites :: SpriteCache            -- cache with all game spirtes
  } 
  | WelcomeScreen
  { keysPressed :: Set Key 
  , paused :: Bool 
  , sprites :: SpriteCache
  } --deriving Show                     -- TODO: debug output

-- | The starting state for the game.
initialState :: GameState
initialState = WelcomeScreen
  { keysPressed = empty
  , paused = False
  , sprites = loadAllSprites
  }

-- | Transfering loaded sprites into GameState
initialLoadedGameState :: SpriteCache -> GameState
initialLoadedGameState loadedSprites = Game  
   { player = Player (0,(-150)) 100 0 (sSpaceshipSprites loadedSprites) noMovement
  -- enemies = []
  -- obstacle = []
  , obstaclesAsteroids = []
  , playerProjectiles = []
  -- enemiesProjectiles = []
  -- timeFromLastAddedEnemy = 0
  , keysPressed = empty
  , paused = False
  , generator = mkStdGen(23456)
  , sprites = loadedSprites
  }
 
{- 
-- | The starting state for the game.
initialGameState :: GameState
initialGameState = Game
  { player = Player (0,(-150)) 100 0 loadSpaceshipSprites noMovement
  -- enemies = []
  -- obstacle = []
  , obstaclesAsteroids = []
  , playerProjectiles = []
  -- enemiesProjectiles = []
  -- timeFromLastAddedEnemy = 0
  , keysPressed = empty
  , paused = False
  , generator = mkStdGen(23456)
  , sprites = loadAllSprites
  }
-}

-- DRAW FUNCTIONS --

drawWelcomeScreen :: Map Char Picture -> Picture
drawWelcomeScreen spriteFont =
  pictures
  [ Scale 2 2 $ rowOrder (-2) $ centerText "SPACE SHOOTER" spriteFont
  , rowOrder 2 $ centerText "Press P to start" spriteFont
  ]
  where
    rowOrder :: Float -> Picture -> Picture
    rowOrder number = translate 0 (fromIntegral (-imageSpriteFontSize) * number)
    center :: Int -> Picture -> Picture
    center length = translate (-(fromIntegral ((length)*imageSpriteFontSize) / 2.0)) 0
    centerText :: [Char] -> Map Char Picture -> Picture
    centerText text font = center (length text) $ makeSpriteText text font
  
drawInfo :: Map Char Picture -> Picture
drawInfo spriteFont = 
  --Scale textScale textScale $
    --color red $
      pictures
      [ rowOrder 1 $ makeSpriteText "Lives:" spriteFont
      , rowOrder 2 $ makeSpriteText "Score:" spriteFont
      --, rowOrder 3 $ makeSpriteText "" spriteFont
      ]
  where
    rowOrder :: Float -> Picture -> Picture
    rowOrder number = translate 0 (fromIntegral (-imageSpriteFontSize) * number)

drawControlsInfo :: Map Char Picture -> Picture
drawControlsInfo spriteFont = 
  --Scale 0.8 0.8 $ 
  pictures
  [ rowROrder 4 $ makeSpriteTextTight "CONTROLS" spriteFont
  , rowROrder 3 $ makeSpriteTextTight "Move :WASD" spriteFont
  , rowROrder 2 $ makeSpriteTextTight "Shoot:Space" spriteFont
  , rowROrder 1 $ makeSpriteTextTight "Pause:P" spriteFont
  , rowROrder 0 $ makeSpriteTextTight "Reset:R" spriteFont
  ]
  where
    rowROrder :: Float -> Picture -> Picture
    rowROrder number = translate 0 (fromIntegral (imageSpriteFontSize) * number)

drawDebugInfo :: GameState -> Picture
drawDebugInfo game = 
  Scale debugTextScale debugTextScale $ 
    color red $ 
      pictures 
      [ rowROrder 7 $ Text "DEBUG INFO"
      , rowROrder 6 $ Text $ "No. of asteroids: " ++ 
          show (length (obstaclesAsteroids game))
      , rowROrder 5 $ Text $ "No. of player's projectiles: " ++ 
          show (length (playerProjectiles game))
      , rowROrder 4 $ Text "Player:"
      , rowROrder 3 $ Text $ debugPlayerPosition (player game) 
      , rowROrder 2 $ Text $ debugPlayerSpeed (player game)
      , rowROrder 1 $ Text $ debugPlayerReloadTime (player game)
      ]
  where
    rowROrder :: Float -> Picture -> Picture
    rowROrder number = translate 0 (textHeight * number)
    
drawPauseScreen :: Map Char Picture -> Picture
drawPauseScreen spriteFont =
  --Scale 0.5 0.5 $
    --translate (-250) 0 $
      --color yellow $
        --Text "PAUSED" 
    centerText "PAUSED" spriteFont       
  where
    --rowOrder :: Float -> Picture -> Picture
    --rowOrder number = translate 0 (fromIntegral (-imageSpriteFontSize) * number)
    center :: Int -> Picture -> Picture
    center length = translate (-(fromIntegral ((length)*imageSpriteFontSize) / 2.0)) 0
    centerText :: [Char] -> Map Char Picture -> Picture
    centerText text font = center (length text) $ makeSpriteText text font
  
-- | Convert a game state into a picture.
render :: GameState  -- ^ The game state to render.
       -> Picture    -- ^ A picture of this game state.
render game@(WelcomeScreen _ _ sprites) = 
  pictures [drawWelcomeScreen (sSpriteFont sprites)]
render game =
  pictures 
  [ translate (iWidth /. 2) 0 $ pictures [walls,projectiles,spaceship,reloadBar, asteroids] -- all game objects
  , translateToInfoSideBar (height /. 2 ) $ drawInfo (sSpriteFont (sprites game))
  , translateToInfoSideBar 0 $ drawControlsInfo (sSpriteFont (sprites game))
  , translateToInfoSideBar (-height /. 2 ) $ drawDebugInfo game
  , if (paused game) then translate (iWidth /. 2) 0 $ drawPauseScreen (sSpriteFont (sprites game)) else Blank
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
        color wallColor $
          rectangleSolid wallBoundWidth (fromIntegral height)

    wallColor = greyN 0.5
    walls = pictures [wall ((width /. 2) - (wallBoundWidth / 2)), 
                      wall (( (-width) /. 2) + (wallBoundWidth / 2))]
    
    -- Player projectiles
    projectiles :: Picture
    projectiles = pictures $ map drawProjectile (playerProjectiles game) 
    
    -- Asteroids
    asteroids :: Picture
    asteroids = pictures $ map drawAsteroid (obstaclesAsteroids game)


-- UPDATE FUNCTIONS -- TODO: Combine all into one update function
    
-- | Calls updatePlayer for GameState
updatePlayerInGame :: Float -> GameState -> GameState
updatePlayerInGame seconds game =
  game { player = updatePlayer (keysPressed game) seconds (player game) }

-- | Update all projectiles fired by player for GameState
updatePlayerProjectilesInGame :: Float -> GameState -> GameState
updatePlayerProjectilesInGame seconds game = 
  game { playerProjectiles = 
           map (updateProjectile seconds) (playerProjectiles game) }
 
 
-- | Update asteroids
updateAsteroidsInGame :: Float -> GameState -> GameState
updateAsteroidsInGame seconds game = 
        game { obstaclesAsteroids = Prelude.map (updateAsteroid seconds) (obstaclesAsteroids game) }

-- | Collision between asteroids and projectiles
handleProjectilesAsteroidsCollision :: Float -> GameState -> GameState
handleProjectilesAsteroidsCollision seconds game =
        game { obstaclesAsteroids = Prelude.map (\x-> snd x) remaindAsteroids
             , playerProjectiles = Prelude.map (\x-> snd x) remaindProjectiles}
        where
        asteroids = zip [1..] $ obstaclesAsteroids game
        projectiles =zip [1..] $ playerProjectiles game 
        asteroidProjectilesList = [(a,p) | a <- asteroids, p <- projectiles]
        asteroidProjectilesListFiltered = Prelude.filter (\x -> checkForAsteoridProjectileCollision  (snd ( fst x)) (snd ( snd x))) asteroidProjectilesList
        asteroidIndicesForRemove = returnAsteroidIndices asteroidProjectilesList asteroidProjectilesListFiltered
        projectileIndicesForRemove = returnProjectileIndices asteroidProjectilesList asteroidProjectilesListFiltered
        remaindAsteroids = Prelude.filter (\x->  (elem (fst x) asteroidIndicesForRemove) == False) asteroids
        remaindProjectiles = Prelude.filter (\x-> (elem (fst x) projectileIndicesForRemove) == False) projectiles
 
 

checkForAsteoridProjectileCollision :: Asteroid -> Projectile -> Bool
checkForAsteoridProjectileCollision asteroid projectile = centerDistance > radiusSum
        where
        (ax, ay) = aPosition asteroid
        aw = aWidth asteroid / 2 --asteroid radius
        (px, py) = rPosition projectile
        pw = 4 --projectile radius
        centerDistance = (ax-px)^2 + (ay-py)^2
        radiusSum = (aw + pw)^2
        
returnAsteroidIndices :: [((Int , Asteroid),(Int, Projectile))] -> [((Int , Asteroid),(Int, Projectile))]->[Int]
returnAsteroidIndices  unfilteredList filteredList = asteroidIndices
        where 
        asteroidsForRemoving = Prelude.filter (\x-> (elem x filteredList) == False) unfilteredList
        asteroidIndices = Prelude.foldl (\acc x -> (fst ( fst x)) : acc) [] asteroidsForRemoving

returnProjectileIndices :: [((Int , Asteroid),(Int, Projectile))] -> [((Int , Asteroid),(Int, Projectile))]->[Int]
returnProjectileIndices  unfilteredList filteredList = projectileIndices
        where 
        projectilesForRemoving = Prelude.filter (\x-> (elem x filteredList) == False) unfilteredList
        projectileIndices = Prelude.foldl (\acc x -> (fst (snd x)) : acc) [] projectilesForRemoving

-- | Add asteroids to the game
addAsteroidsToGame :: Float -> GameState -> GameState
addAsteroidsToGame seconds game =
    game { obstaclesAsteroids = newObstaclesAsteroids, generator = gen''''' }
    where
    oldObstaclesAsteroids = obstaclesAsteroids game
    gen = generator game
    (x', gen') = randomR ((-width /.2 ) + wallBoundWidth + 32.0, (width /. 2) - wallBoundWidth - 32.0) gen :: (Float, StdGen) 
    y'= (height /. 2) - 2.0;
    (step,gen'') = randomR (1,600) gen' ::(Int, StdGen)
    (speedX, gen''') = randomR (lowestAsteroidSpeedX, highestAsteroidSpeedX) gen'' ::(Float, StdGen)
    (speedY, gen'''') = randomR (lowestAsteroidSpeedY,highestAsteroidSpeedY) gen''' ::(Float, StdGen)
    (deg, gen''''') = randomR (15,30) gen''' ::(Float, StdGen)
    newObstaclesAsteroids = if (step>598) 
                            then (Asteroid (x',y') 32.0 (speedX,speedY) deg (sAsteroidSprite (sprites game))) : oldObstaclesAsteroids
                            else oldObstaclesAsteroids

-- | Player fired a projectile
projectileFiredByPlayer :: GameState -> GameState
projectileFiredByPlayer game = 
  if (  (member (SpecialKey KeySpace) (keysPressed game)) -- is the key for firing a projectile being held
     && (canFireProjectile (player game))  -- has the player reloaded a projectile
     )
  then -- fire
    game { playerProjectiles = addProjectile (px,py') (0,projectileSpeed) animation (playerProjectiles game)
         , player = reload (player game) -- reload after firing  
         }
  else
    game
  where 
    (px,py) = pPosition (player game)
    py' = py + shipSizeHt
    animation :: SpriteAnimation
    animation = makeRepeatingAnimation projectileSpriteChangeInterval (sProjectileSprites (sprites game))

-- | Deletes all out of bounds projectiles from game
deleteProjectilesFormGame :: GameState -> GameState
deleteProjectilesFormGame game =
  game { playerProjectiles = 
           deleteOutOfBoundsProjectiles (playerProjectiles game) }

-- -- | Deletes all out of bounds asteroids from game
deleteAsteroidsFromGame :: GameState -> GameState
deleteAsteroidsFromGame game =
    game { obstaclesAsteroids = 
         deleteOutOfBoundsAsteroids (obstaclesAsteroids game) }


-- | Update the game
update :: Float -> GameState -> GameState 
update _ game@(WelcomeScreen _ _ loadedSprites) = if not (paused game)
  then game
  else initialLoadedGameState loadedSprites
update seconds game = if not (paused game) 
                      then 
                        projectileFiredByPlayer .
                        updatePlayerProjectilesInGame seconds . 
                        deleteProjectilesFormGame .
                        updateAsteroidsInGame seconds .
                        addAsteroidsToGame seconds .
                        deleteAsteroidsFromGame .
                        handleProjectilesAsteroidsCollision seconds .
                        updatePlayerInGame seconds $ 
                        game
                      else 
                        game




-- INPUT FUNCTIONS --

-- | Respond to key events.
handleKeys :: Event -> GameState -> GameState

-- For an 'p' keypress, pause the game
handleKeys (EventKey (Char 'p') Down _ _) game =
  game { paused = not (paused game) }

-- Ignore 'p' keypress release
handleKeys (EventKey (Char 'p') Up _ _) game = game

-- For an 'r' keypress, reset the game
handleKeys (EventKey (Char 'r') Down _ _) game = initialState

-- Ignore 'r' keypress release
handleKeys (EventKey (Char 'r') Up _ _) game = game

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
