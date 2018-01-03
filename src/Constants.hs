module Constants where
  
-- | Windows size constants
width, height, offset :: Int
width = 400 
height = 600
offset = 100

-- | Info sidebar constants
iWidth :: Int
iWidth = 200
textScale, controlsTextScale, debugTextScale, textHeight :: Float
textScale = 0.25
controlsTextScale = 0.1
debugTextScale = 0.1
textHeight = 125

-- | Game objects constants
wallBoundWidth, shipSizeWh, shipSizeHt, shipSizeHb, playerReloadTime :: Float
wallBoundWidth = 10     -- width of surrounding walls
shipSizeWh = 15         -- half of spaceship's width
shipSizeHt = 25         -- spaceship's length of front end
shipSizeHb = 15         -- spaceship's length of back end
playerReloadTime = 0.5  -- minimal time between fired projectile 

-- | Number of frames to show per second.
fps :: Int
fps = 60

-- | Asteroids
lowestAsteroidSpeedX, highestAsteroidSpeedX, lowestAsteroidSpeedY, highestAsteroidSpeedY :: Float
lowestAsteroidSpeedX = -40.0
highestAsteroidSpeedX = 40.0
lowestAsteroidSpeedY = -40.0
highestAsteroidSpeedY = -10.0
-- | Images of objects.
imageOfAsteroid :: String
imageOfAsteroid = "C:/Users/Kolibri/FP/FP006-space-shooter/images/asteroid_day_favicon.png"