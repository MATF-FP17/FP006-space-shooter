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
enemyProjectileDamageToPlayer :: Int --percent of damage taken when hit by an enemy projectile
enemyProjectileDamageToPlayer = 10
asteroidDestructionScore , enemyDestructionScore :: Int
asteroidDestructionScore = 5
enemyDestructionScore = 10
enemySpawnTime :: Float 
enemySpawnTime = 2
enemyReloadTime, enemyInitialReloadTime :: Float
enemyReloadTime = 1.9
enemyInitialReloadTime = 3

-- | Images of objects.
imageOfAsteroidSmall :: String
imageOfAsteroidSmall = "C:/Users/Kolibri/FP/FP006-space-shooter/images/asteroid_day_favicon.png"
widthAsteroidSmall :: Float
widthAsteroidSmall = 32.0

imageOfAsteroidBig :: String
imageOfAsteroidBig = "C:/Users/Kolibri/FP/FP006-space-shooter/images/asteroid_big.png"

widthAsteroidBig :: Float
widthAsteroidBig = 48.0

imageShipN0, imageShipN1, imageShipN2, imageShipL0, imageShipL1, imageShipL2, imageShipR0, imageShipR1, imageShipR2 :: String
imageShipN0 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/ship_normal_2.png"
imageShipN1 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/ship_normal_1.png"
imageShipN2 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/ship_normal_0.png"
imageShipL0 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/ship_left_2.png"
imageShipL1 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/ship_left_1.png"
imageShipL2 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/ship_left_0.png"
imageShipR0 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/ship_right_2.png"
imageShipR1 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/ship_right_1.png"
imageShipR2 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/ship_right_0.png"
imageShipHeight, imageShipWidth :: Int
imageShipHeight = 43
imageShipWidth = 39
imageProjectileA1, imageProjectileA2, imageProjectileA3, imageProjectileA4 :: String 
imageProjectileA5, imageProjectileA6, imageProjectileA7, imageProjectileA8 :: String
imageProjectileB1, imageProjectileB2, imageProjectileB3, imageProjectileB4 :: String
imageProjectileB5, imageProjectileB6, imageProjectileB7, imageProjectileB8 :: String
imageProjectileA1 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/projectile_A1.png" 
imageProjectileA2 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/projectile_A2.png" 
imageProjectileA3 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/projectile_A3.png" 
imageProjectileA4 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/projectile_A4.png" 
imageProjectileA5 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/projectile_A5.png" 
imageProjectileA6 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/projectile_A6.png" 
imageProjectileA7 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/projectile_A7.png" 
imageProjectileA8 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/projectile_A8.png" 
imageProjectileB1 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/projectile_B1.png" 
imageProjectileB2 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/projectile_B2.png" 
imageProjectileB3 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/projectile_B3.png" 
imageProjectileB4 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/projectile_B4.png" 
imageProjectileB5 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/projectile_B5.png" 
imageProjectileB6 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/projectile_B6.png" 
imageProjectileB7 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/projectile_B7.png" 
imageProjectileB8 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/projectile_B8.png" 
imageProjectileSize :: Int
imageProjectileSize = 13
projectileSpriteNumber :: Int
projectileSpriteNumber = 8

imageEnemyNormal, imageEnemyLeft, imageEnemyRight :: String
imageEnemyNormal = "C:/Users/Kolibri/FP/FP006-space-shooter/images/enemyNormal.png"
imageEnemyLeft = "C:/Users/Kolibri/FP/FP006-space-shooter/images/enemyLeft.png"
imageEnemyRight = "C:/Users/Kolibri/FP/FP006-space-shooter/images/enemyRight.png"
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
imageSpriteFont0 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/0.png"
imageSpriteFont1 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/1.png"
imageSpriteFont2 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/2.png"
imageSpriteFont3 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/3.png"
imageSpriteFont4 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/4.png"
imageSpriteFont5 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/5.png"
imageSpriteFont6 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/6.png"
imageSpriteFont7 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/7.png"
imageSpriteFont8 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/8.png"
imageSpriteFont9 = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/9.png"
imageSpriteFontA = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/a.png"
imageSpriteFontB = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/b.png"
imageSpriteFontC = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/c.png"
imageSpriteFontD = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/d.png"
imageSpriteFontE = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/e.png"
imageSpriteFontF = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/f.png"
imageSpriteFontG = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/g.png"
imageSpriteFontH = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/h.png"
imageSpriteFontI = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/i.png"
imageSpriteFontJ = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/j.png"
imageSpriteFontK = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/k.png"
imageSpriteFontL = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/l.png"
imageSpriteFontM = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/m.png"
imageSpriteFontN = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/n.png"
imageSpriteFontO = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/o.png"
imageSpriteFontP = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/p.png"
imageSpriteFontQ = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/q.png"
imageSpriteFontR = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/r.png"
imageSpriteFontS = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/s.png"
imageSpriteFontT = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/t.png"
imageSpriteFontU = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/u.png"
imageSpriteFontV = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/v.png"
imageSpriteFontW = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/w.png"
imageSpriteFontX = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/x.png"
imageSpriteFontY = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/y.png"
imageSpriteFontZ = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/z.png"
imageSpriteFontComma = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/!c.png"
imageSpriteFontDot = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/!d.png"
imageSpriteFontDotComma = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/!dc.png"
imageSpriteFontDoubleDot = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/!dd.png"
imageSpriteFontEqual = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/!e.png"
imageSpriteFontExclamation = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/!ex.png"
imageSpriteFontMinus = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/!m.png"
imageSpriteFontPlus = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/!p.png"
imageSpriteFontQuestion = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/!q.png"
imageSpriteFontSpace = "C:/Users/Kolibri/FP/FP006-space-shooter/images/font/!s.png"

