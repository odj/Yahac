{-# LANGUAGE FlexibleInstances #-}
module Yahac.Asteroids.Internal.Point (
    Point,
    DoubleVector(..),
    unitVector
) where

type Point = (Double, Double) 



class DoubleVector a where
    (<+>) :: a -> a -> a
    (<->) :: a -> a -> a
    (<.>) :: a -> a -> Double
    (<*>) :: a -> Double -> a
    (</>) :: a -> Double -> a
    dist  :: a -> a -> Double
    dist2 :: a -> a -> Double
    unit  :: a -> a -> a

instance DoubleVector Point where
    (x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)
    (x1, y1) <-> (x2, y2) = (x1 - x2, y1 - y2)
    (x1, y1) <.> (x2, y2) = (x1 * x2 + y1 * y2)
    (x1, y1) <*> s        = (x1 * s, y1 * s)
    (x1, y1) </> s        | s == 0.0 = (x1, y1) | otherwise = (x1 / s, y1 / s)
    dist2 (x1, y1) (x2, y2) = (x2 - x1) ^2 + (y2 - y1) ^2
    dist p1 p2 = sqrt $ dist2 p1 p2
    unit p2 p1 = (p2 <-> p1) </> (dist p2 p1)


-- | A unit vector with angle theta
unitVector theta = (sin (theta + pi), cos (theta))




