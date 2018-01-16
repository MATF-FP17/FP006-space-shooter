module ObjectCollision
  ( translatePoly
  , translatePoint
  , scalePoint
  , scalePoly
  , polyFrom
  , linesFrom
  , circleIntersectingPoly
  , pointDistanceSq
  , distancePointToLineSq
  , insidePoly
  , pointLeftOfLine
  , circleRectangleCollision
  ) where

import Data.List

type   Pt a = (a, a)       -- Point (x coordinate, y coordinate)
type   Ln a = (Pt a, Pt a) -- Line Segment (start point, end point)
type Poly a = [Pt a]       -- Polygon (list of points)
type Circ a = (Pt a, a)    -- Circle (center point, radius)
type Rect a = (Pt a, a, a) -- Rectangle (center point, width, height)
 
-- | Scale all points in a polygon
translatePoly :: (Ord a, Floating a) => a -> a -> Poly a -> Poly a
translatePoly x y poly = map (translatePoint x y) poly
 
-- | Scale a single point
translatePoint :: (Ord a, Floating a) => a -> a -> Pt a -> Pt a
translatePoint x y (px,py) = (px+x,py+y)
    
-- | Scale all points in a polygon
scalePoly :: (Ord a, Floating a) => a -> a -> Poly a -> Poly a
scalePoly x y poly = map (scalePoint x y) poly
 
-- | Scale a single point
scalePoint :: (Ord a, Floating a) => a -> a -> Pt a -> Pt a
scalePoint x y (px,py) = (x*px,y*py)
 
-- | Return a polygon from a list of points.
polyFrom poly = last poly : poly
 
-- | Return a list of lines from a list of points.
linesFrom poly@(_:ps) = zip poly ps

-- Checks if circle is intersecting with polygon
circleIntersectingPoly :: (Ord a, Floating a) => Circ a -> Poly a -> Bool
circleIntersectingPoly circ@(center,_) poly =
  inVertexRegion circ poly ||
  inEdgeRegion circ (linesFrom poly) || 
  insidePoly center poly
  
inVertexRegion :: (Ord a, Floating a) => Circ a -> Poly a -> Bool
inVertexRegion (c,cr) poly = any (closePoints cr c) poly

closePoints :: (Ord a, Floating a) => a -> Pt a -> Pt a -> Bool
closePoints dist p1 p2 = pointDistanceSq p1 p2 <= (dist)^2
  
-- Returns squared distance between two points
pointDistanceSq :: (Ord a, Floating a) => Pt a -> Pt a -> a
pointDistanceSq (x1,y1) (x2,y2) = (x1-x2)^2 + (y1-y2)^2
  
inEdgeRegion :: (Ord a, Floating a) => Circ a -> [Ln a] -> Bool
inEdgeRegion (c,cr) lines = any (closeToLine cr c) lines

closeToLine :: (Ord a, Floating a) => a -> Pt a -> Ln a -> Bool
closeToLine dist circ line = distancePointToLineSq circ line <= dist^2
  
-- | Returns squared distance between a point and a line segment
distancePointToLineSq :: (Ord a, Floating a) => Pt a -> Ln a -> a
distancePointToLineSq (x,y) ((x1,y1),(x2,y2)) = 
  let dot = ((x-x1)*(x2-x1)) + ((y-y1)*(y2-y1))
      len_sq = (x2-x1)^2 + (y2-y1)^2
      param = if len_sq == 0 then 0 else (min 1 $ max 0 (dot / len_sq))
      dx = x1 + param*(x2-x1)
      dy = y1 + param*(y2-y1)
      dist_sq = (x-dx)^2 + (y-dy)^2
      --dist_sqrt = sqrt dist_sq
  in dist_sq
  
-- | Returns true if point is inside polygon
insidePoly :: (Ord a, Floating a) => Pt a -> Poly a -> Bool
insidePoly point poly = all (pointLeftOfLine point) (linesFrom poly)

-- | Returns true if point (x,y) is on the left side of the oriented line (px,py),(qx,qy))
pointLeftOfLine :: (Ord a, Num a) => Pt a -> Ln a -> Bool
pointLeftOfLine (x,y) ((px,py),(qx,qy)) = (qx-px)*(y-py) >= (qy-py)*(x-px)

-- | Checks if there is collision between given circle and rectangle
circleRectangleCollision :: (Ord a, Floating a) => Circ a -> Rect a -> Bool
circleRectangleCollision ((cx,cy),cr) ((rx,ry),rw,rh) =
  if (distanceX > (rw2+cr)) then False
  else if (distanceY > (rh2+cr)) then False
  else if (distanceX <= rw2) then True
  else if (distanceY <= rh2) then True
  else ( ((distanceX-rw2)^2 + (distanceY-rh2)^2) <= (cr^2) )
  where 
    rw2 = rw / 2.0
    rh2 = rh / 2.0
    distanceX = abs (cx-rx)
    distanceY = abs (cy-ry)
