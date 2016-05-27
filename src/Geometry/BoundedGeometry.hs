module Geometry.BoundedGeometry where

import Utils
import Geometry.Geometry

data Bound p = AABB (Pos p) (Pos p) -- ^ Axis Aligned Bounding Box with TopLeft and BottomRight
             | EncCircle (Pos p) p  -- ^ Enclosing Circle with center and radius squared
             deriving (Eq, Show)

data BoundedGeometry p = BSeg (Bound p) (Seg p)
                       -- |
