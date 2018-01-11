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
enemySizeH, enemySizeW :: Float
enemySizeW = 40.0       -- enemy's width in game
enemySizeH = 40.0       -- enemy's height in game
projectileRadius :: Float
projectileRadius = 4    -- radius of all ingame projectiles

-- | Number of frames to show per second.
fps :: Int
fps = 60

-- | Asteroids
lowestAsteroidSpeedX, highestAsteroidSpeedX, lowestAsteroidSpeedY, highestAsteroidSpeedY :: Float
lowestAsteroidSpeedX = -40.0
highestAsteroidSpeedX = 40.0
lowestAsteroidSpeedY = -40.0
highestAsteroidSpeedY = -10.0

-- | Object interaction and Gameplay
asteroidDamageToPlayer :: Int --percent of damage taken when colliding with asteroid
asteroidDamageToPlayer = 20
asteroidDestructionScore , enemyDestructionScore :: Int
asteroidDestructionScore = 5
enemyDestructionScore = 10
enemySpawnTime :: Float 
enemySpawnTime = 2

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

imageEnemyNormal, imageEnemyLeft, imageEnemyRight :: String
imageEnemyNormal = "images/enemyNormal.png"
imageEnemyLeft = "images/enemyLeft.png"
imageEnemyRight = "images/enemyRight.png"
imageEnemyHeight, imageEnemyWidth :: Int
imageEnemyHeight = 30
imageEnemyWidth = 28

-- | Animation
--spaceshipSpriteChangeInterval :: Float
--spaceshipSpriteChangeInterval = 0.200 -- time interval between sprites
projectileSpriteChangeInterval :: Float
projectileSpriteChangeInterval = 0.1 -- time interval between sprites

-- | SpriteFont
imageSpriteFontSize :: Int
imageSpriteFontSize = 20
imageSpriteFont0 = "images/font/0.png"
imageSpriteFont1 = "images/font/1.png"
imageSpriteFont2 = "images/font/2.png"
imageSpriteFont3 = "images/font/3.png"
imageSpriteFont4 = "images/font/4.png"
imageSpriteFont5 = "images/font/5.png"
imageSpriteFont6 = "images/font/6.png"
imageSpriteFont7 = "images/font/7.png"
imageSpriteFont8 = "images/font/8.png"
imageSpriteFont9 = "images/font/9.png"
imageSpriteFontA = "images/font/a.png"
imageSpriteFontB = "images/font/b.png"
imageSpriteFontC = "images/font/c.png"
imageSpriteFontD = "images/font/d.png"
imageSpriteFontE = "images/font/e.png"
imageSpriteFontF = "images/font/f.png"
imageSpriteFontG = "images/font/g.png"
imageSpriteFontH = "images/font/h.png"
imageSpriteFontI = "images/font/i.png"
imageSpriteFontJ = "images/font/j.png"
imageSpriteFontK = "images/font/k.png"
imageSpriteFontL = "images/font/l.png"
imageSpriteFontM = "images/font/m.png"
imageSpriteFontN = "images/font/n.png"
imageSpriteFontO = "images/font/o.png"
imageSpriteFontP = "images/font/p.png"
imageSpriteFontQ = "images/font/q.png"
imageSpriteFontR = "images/font/r.png"
imageSpriteFontS = "images/font/s.png"
imageSpriteFontT = "images/font/t.png"
imageSpriteFontU = "images/font/u.png"
imageSpriteFontV = "images/font/v.png"
imageSpriteFontW = "images/font/w.png"
imageSpriteFontX = "images/font/x.png"
imageSpriteFontY = "images/font/y.png"
imageSpriteFontZ = "images/font/z.png"
imageSpriteFontComma = "images/font/!c.png"
imageSpriteFontDot = "images/font/!d.png"
imageSpriteFontDotComma = "images/font/!dc.png"
imageSpriteFontDoubleDot = "images/font/!dd.png"
imageSpriteFontEqual = "images/font/!e.png"
imageSpriteFontExclamation = "images/font/!ex.png"
imageSpriteFontMinus = "images/font/!m.png"
imageSpriteFontPlus = "images/font/!p.png"
imageSpriteFontQuestion = "images/font/!q.png"
imageSpriteFontSpace = "images/font/!s.png"

