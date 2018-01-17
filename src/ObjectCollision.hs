module ObjectCollision
  ( Pt, Ln, Poly, Circ, Rect
  , translatePoly
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
  , clipTo
  , circleRectangleCollision
  ) where

import Data.List

type   Pt a = (a, a)       -- Point (x coordinate, y coordinate)
type   Ln a = (Pt a, Pt a) -- Line Segment (start point, end point)
type Poly a = [Pt a]       -- Polygon (list of points)
type Circ a = (Pt a, a)    -- Circle (center point, radius)
type Rect a = (Pt a, a, a) -- Rectangle (center point, width, height)
 
-- | Translate all points in a polygon
translatePoly :: (Ord a, Floating a) => a -> a -> Poly a -> Poly a
translatePoly x y poly = map (translatePoint x y) poly
 
-- | Translate a single point
translatePoint :: (Ord a, Floating a) => a -> a -> Pt a -> Pt a
translatePoint x y (px,py) = (px+x,py+y)
    
-- | Scale all points in a polygon
scalePoly :: (Ord a, Floating a) => a -> a -> Poly a -> Poly a
scalePoly x y poly = map (scalePoint x y) poly
 
-- | Scale a single point
scalePoint :: (Ord a, Floating a) => a -> a -> Pt a -> Pt a
scalePoint x y (px,py) = (x*px,y*py)
 

-- | Returns polygon from list of points
polyFrom [] = []
polyFrom points = last points : points
 
-- | Return a list of lines from a list of points.
linesFrom [] = []
linesFrom poly@(_:ps) = zip poly ps

-- | Return true if the point (x,y) is on or to the left of the oriented line defined by (px,py) and (qx,qy).
pointLeftOfLine :: (Ord a, Num a) => Pt a -> Ln a -> Bool
pointLeftOfLine (x,y) ((px,py),(qx,qy)) = (qx-px)*(y-py) >= (qy-py)*(x-px)

-- Return the intersection of two lines.
intersectionOfTwoLines :: Fractional a => Ln a -> Ln a -> Pt a
intersectionOfTwoLines ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) =
    let (r,s) = (x1*y2-y1*x2, x3*y4-y3*x4)
        (t,u,v,w) = (x1-x2, y3-y4, y1-y2, x3-x4)
        d = t*u-v*w 
    in ((r*w-t*s)/d, (r*u-v*s)/d)

-- Intersect the line segment (p0,p1) with the clipping line's left halfspace,
-- returning the point closest to p1.  In the special case where p0 lies outside
-- the halfspace and p1 lies inside we return both the intersection point and
-- p1.  This ensures we will have the necessary segment along the clipping line.
intersectionLineClippingLine :: (Fractional a, Ord a) => Ln a -> Ln a -> [Pt a]
intersectionLineClippingLine ln@(p0, p1) clipLn =
    case (pointLeftOfLine p0 clipLn, pointLeftOfLine p1 clipLn) of
      (False, False) -> []
      (False, True)  -> [isect, p1]
      (True,  False) -> [isect]
      (True,  True)  -> [p1]
    where isect = intersectionOfTwoLines ln clipLn

-- Intersect the polygon with the clipping line's left halfspace.
intersectionPolyClippingLine :: (Fractional a, Ord a) => Poly a -> Ln a -> Poly a
intersectionPolyClippingLine poly clipLn = polyFrom $ concatMap (\x ->intersectionLineClippingLine x clipLn) (linesFrom poly)
 
-- Intersect a target polygon with a clipping polygon.  The latter is assumed to
-- be convex.
clipTo :: (Fractional a, Ord a) => [Pt a] -> [Pt a] -> [Pt a]
targPts `clipTo` clipPts = 
    let targPoly = polyFrom targPts
        clipLines = linesFrom (polyFrom clipPts)
    in foldl' (intersectionPolyClippingLine) targPoly clipLines

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
