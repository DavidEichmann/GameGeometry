{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}

module Geometry.Angles where

import Utils
import Data.Fixed
import Geometry.Geometry
import Linear

newtype Spr1 a = Spr1 { unSpr1 :: a } deriving (Show, Eq, Ord)   -- ^ original definition of spread. In the range [0, 1]
newtype Spr a = Spr { unSpr :: a } deriving (Show, Eq, Ord)     -- ^ spread over the 4 quadrants. In the range [0, 4)

data AngleWedge a = AngleWedge (Spr a) (Spr a)

angleWedge :: Real a =>  Spr a -> Spr a -> AngleWedge a
angleWedge (Spr lo) (Spr hi) = AngleWedge
                                        (Spr (lo `mod'` 4))
                                        (Spr (hi `mod'` 4))

angleWedgeFromPointAndSeg :: RealFrac a => Pos a -> Seg a -> Maybe (AngleWedge a)
angleWedgeFromPointAndSeg focus (Seg' a b) = if c == 0 then Nothing else Just (angleWedge' (spreadX2 fa) (spreadX2 fb))
    where
        fa = a - focus
        fb = b - focus
        ab = b - a
        c = fa `crossZ` ab
        angleWedge' = if c > 0 then AngleWedge else flip AngleWedge

-- | spread (sin^2) of an angle with the x-axis. This is modified to give a
--   smooth transition from 0 to 4 throughout the 4 quadrants.
spreadX2 :: (Fractional a, Ord a) => V2 a -> Spr a
spreadX2 v@(V2 x y)
    | x == 0 = if
        | y > 0     -> Spr 1
        | y < 0     -> Spr 3
        | otherwise -> Spr 0
    | y == 0 = if
        | x > 0     -> Spr 0
        | x < 0     -> Spr 2
        | otherwise -> Spr 0
    | x > 0 = if
        | y > 0     -> Spr s
        | y < 0     -> Spr (4 - s)
    | x < 0 = if
        | y > 0     -> Spr (2 - s)
        | y < 0     -> Spr (s + 2)
    where
        Spr1 s = spreadX v

spreadX :: Fractional a => V2 a -> Spr1 a
spreadX v@(V2 _ y) = Spr1 (q / r)
    where
        r = quadrance v
        q = y ^^ 2

-- can check by XORing (use (/=)) the three orderings
-- this is inclusive
-- TODO benchmark and improve
isOnSpr :: Ord p => Spr p -> Spr p -> Spr p -> Bool
isOnSpr point start end = point == start || point == end || (((point <= start) /= (point <= end)) /= (start >= end))

-- ccwDist :: Spr p -> Spr p -> Ordering
-- ccwDist (Spr a) (Spr b) = a - b
