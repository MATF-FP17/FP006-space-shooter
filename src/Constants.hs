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
wallBoundWidth, shipSizeWh, shipSizeHt, shipSizeHb, shipSizeHbTail, playerReloadTime, projectileSpeed :: Float
wallBoundWidth = 10     -- width of surrounding walls
shipSizeWh = 20         -- half of spaceship's width
shipSizeHt = 20         -- spaceship's length of front end
shipSizeHb = 15         -- spaceship's length of back end
shipSizeHbTail = 8      -- spaceship's back end ignored (for drawing fuel trail)
playerReloadTime = 0.5  -- minimal time between fired projectile 
projectileSpeed = 200   -- projectile default speed

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
imageOfAsteroid = "images/asteroid_day_favicon.png"
imageShipN0, imageShipN1, imageShipN2, imageShipL0, imageShipL1, imageShipL2, imageShipR0, imageShipR1, imageShipR2 :: String
imageShipN0 = "images/ship_normal_2.png"
imageShipN1 = "images/ship_normal_1.png"
imageShipN2 = "images/ship_normal_0.png"
imageShipL0 = "images/ship_left_2.png"
imageShipL1 = "images/ship_left_1.png"
imageShipL2 = "images/ship_left_0.png"
imageShipR0 = "images/ship_right_2.png"
imageShipR1 = "images/ship_right_1.png"
imageShipR2 = "images/ship_right_0.png"
imageShipHeight, imageShipWidth :: Int
imageShipHeight = 43
imageShipWidth = 39
--spaceshipSpriteNumber :: Int
--spaceshipSpriteNumber = 2

-- | Animation
--spaceshipSpriteChangeInterval :: Float
--spaceshipSpriteChangeInterval = 0.200 -- time interval between sprites

