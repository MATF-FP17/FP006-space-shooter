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
imageProjectileA1, imageProjectileA2, imageProjectileA3, imageProjectileA4 :: String 
imageProjectileA5, imageProjectileA6, imageProjectileA7, imageProjectileA8 :: String
imageProjectileB1, imageProjectileB2, imageProjectileB3, imageProjectileB4 :: String
imageProjectileB5, imageProjectileB6, imageProjectileB7, imageProjectileB8 :: String
imageProjectileA1 = "images/projectile_A1.png" 
imageProjectileA2 = "images/projectile_A2.png" 
imageProjectileA3 = "images/projectile_A3.png" 
imageProjectileA4 = "images/projectile_A4.png" 
imageProjectileA5 = "images/projectile_A5.png" 
imageProjectileA6 = "images/projectile_A6.png" 
imageProjectileA7 = "images/projectile_A7.png" 
imageProjectileA8 = "images/projectile_A8.png" 
imageProjectileB1 = "images/projectile_B1.png" 
imageProjectileB2 = "images/projectile_B2.png" 
imageProjectileB3 = "images/projectile_B3.png" 
imageProjectileB4 = "images/projectile_B4.png" 
imageProjectileB5 = "images/projectile_B5.png" 
imageProjectileB6 = "images/projectile_B6.png" 
imageProjectileB7 = "images/projectile_B7.png" 
imageProjectileB8 = "images/projectile_B8.png" 
imageProjectileSize :: Int
imageProjectileSize = 13
projectileSpriteNumber :: Int
projectileSpriteNumber = 8

-- | Animation
--spaceshipSpriteChangeInterval :: Float
--spaceshipSpriteChangeInterval = 0.200 -- time interval between sprites
projectileSpriteChangeInterval :: Float
projectileSpriteChangeInterval = 0.1 -- time interval between sprites

