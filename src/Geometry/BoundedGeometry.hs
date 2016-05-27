module Geometry.BoundedGeometry (
          Bound           (..)
        , BoundedGeometry (..)

        , polygon
        , ngon
    ) where

import Linear
import Utils
import Geometry.Geometry

data Bound p = AABB (Pos p) (Pos p) -- ^ Axis Aligned Bounding Box with BottomLeft and TopRight
             | EncCircle (Pos p) p  -- ^ Enclosing Circle with center and radius squared
             deriving (Eq, Show)

data BoundedGeometry p = BSeg (Bound p) (Seg p) -- This feels useless (probably no perfomance gain)
                       | BPolygon (Bound p) (Polygon p)


polygon :: (Num p, Ord p) => [Pos p] -> BoundedGeometry p
polygon ps = BPolygon (uncurry AABB . aabb $ ps) (Polygon ps)
    where
        aabb :: (Num p, Ord p) => [Pos p] -> (Pos p, Pos p)
        aabb []          = (zero, zero)
        aabb [p]         = (p, p)
        aabb ((V2 x y):ps) = let (V2 l b, V2 r t) = aabb ps in (V2 (min l x) (min b y), V2 (max r x) (max t y))


ngon :: Floating p => Int -> p -> Pos p -> BoundedGeometry p
ngon n radius pos = BPolygon enc (Polygon ps)
    where
        enc = EncCircle pos (radius^2)
        angleN i = 2 * pi * (fromIntegral i) / (fromIntegral n)
        ps = [pos + radius *^ V2 (cos $ angleN i) (sin $ angleN i) | i <- [0..n-1]]
