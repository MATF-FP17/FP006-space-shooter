module Constants
  ( width
  , height
  , offset
  , wallBoundWidth
  , shipSizeWh
  , shipSizeHt
  , shipSizeHb
  , fps
  ) where
  
-- | Windows size constants
width, height, offset :: Int
width = 400 
height = 600
offset = 100

-- | Game objects constants
wallBoundWidth, shipSizeWh, shipSizeHt, shipSizeHb :: Float
wallBoundWidth = 20
shipSizeWh = 15   -- half of spaceship's width
shipSizeHt = 25   -- spaceship's length of front end
shipSizeHb = 15   -- spaceship's length of back end

-- | Number of frames to show per second.
fps :: Int
fps = 60