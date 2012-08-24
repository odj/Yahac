module Yahac.Asteroids.Internal.BoundingBox (
    BoundingBox(..),
    toBoundingBox,
    bbIntersects
) where

import Yahac.Asteroids.Internal.Point

-- | A bounding box
data (DoubleVector a) => BoundingBox a = BoundingBox
    { bbMin :: a
    , bbMax :: a
    } deriving Show

bb0 [] = BoundingBox (0.0, 0.0) (0.0, 0.0) :: BoundingBox Point
bb0 (p0:_) = BoundingBox p0 p0


-- | Creates a bounding box for a list of points
{-# INLINE toBoundingBox #-}
toBoundingBox points = foldl expand (bb0 points) points where
    expand bb (x, y) = BoundingBox (minX, minY) (maxX, maxY) where
        minX | (<) x $ fst $ bbMin bb = x | otherwise = fst $ bbMin bb
        minY | (<) y $ snd $ bbMin bb = y | otherwise = snd $ bbMin bb
        maxX | (>) x $ fst $ bbMax bb = x | otherwise = fst $ bbMax bb
        maxY | (>) y $ snd $ bbMax bb = y | otherwise = snd $ bbMax bb

{-# INLINE isInside #-}
isInside (BoundingBox (ax0, ay0) (ax1, ay1)) (BoundingBox (bx0, by0) (bx1, by1)) = intersects where
    intersects = minInside || maxInside
    minInside =  ax0 < bx0 &&
                 ax1 > bx0 &&
                 ay0 < by0 &&
                 ay1 > by0

    maxInside =  ax0 < bx1 &&
                 ax1 > bx1 &&
                 ay0 < by1 &&
                 ay1 > by1

-- | Tests is two bounding boxes intersect or if one is fully contained by the other
{-# INLINE bbIntersects #-}
bbIntersects bb1 bb2 = (isInside bb1 bb2) || (isInside bb2 bb1)
    
    
