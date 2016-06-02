{-# LANGUAGE ScopedTypeVariables #-}

module Geometry.BoundedGeometry (
          Bound           (..)
        , BoundedGeometry (..)

        -- , polygon
        , ngon
    ) where

import qualified Geometry.Geometry as G
import           Linear
import           Utils

data Bound p = Unbounded
             | AABB (Pos p) (Pos p) -- ^ Axis Aligned Bounding Box with BottomLeft and TopRight
             | EncCircle (Pos p) p  -- ^ Enclosing Circle with center and radius squared
             deriving (Eq, Show)

data BoundedGeometry p = BSeg (Bound p) (G.Seg p) -- This feels useless (probably no perfomance gain)
                       | BPolygon (Bound p) (G.Polygon p)
                       | Tree (Bound p) (BoundedGeometry p) (BoundedGeometry p)


polygon :: forall p. (Eq p, Num p, Ord p) => [Pos p] -> BoundedGeometry p
polygon ps = BPolygon (aabb ps) (G.polygon ps)
    where
        aabb :: [Pos p] -> Bound p
        aabb []            = warning "empty aabb in Geometry.BoundedGeometry.polygon" $ AABB zero zero
        aabb [p]           = AABB p p
        aabb ((V2 x y):ps) =
            let (AABB (V2 l b) (V2 r t)) = aabb ps
            in  AABB (V2 (min l x) (min b y)) (V2 (max r x) (max t y))


ngon :: forall p. (Eq p, Floating p) => Int -> p -> Pos p -> BoundedGeometry p
ngon n radius pos = BPolygon enc (G.polygon ps)
    where
        enc :: Bound p
        enc = EncCircle pos (radius^2)

        angleN :: Int -> p
        angleN i = 2 * pi * (fromIntegral i) / (fromIntegral n)

        ps :: [Pos p]
        ps = [pos + radius *^ V2 (cos $ angleN i) (sin $ angleN i) | i <- [0..n-1]]

depth :: BoundedGeometry p -> Int
depth (BSeg _ _)     = 1
depth (BPolygon _ _) = 1
depth (Tree _ a b)   = 1 + max (depth a) (depth b)

-- geometries :: [BoundedGeometry p] -> BoundedGeometry p

