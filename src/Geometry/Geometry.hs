{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveAnyClass #-}

module Geometry.Geometry ( 

          Line (..)
        , Ray  (..)
        , Seg  (..)

        , crossZ

        , LineIntersect (..)
        , lineIntersection

        , LineRayIntersect (..)
        , lineRayIntersection

        , LineSegIntersect (..)
        , lineSegIntersection
        
        , SegIntersect (..)
        , segIntersection

        , SegRayIntersect (..)
        , segRayIntersection



    ) where

import Data.List
import Linear
import Types

import Test.QuickCheck
import Control.DeepSeq
import GHC.Generics (Generic)

data Line p = Line  (Pos p) (Vec p) deriving (Show, Read, Generic, NFData)
data Ray  p = Ray   (Pos p) (Vec p) deriving (Show, Read, Generic, NFData)
data Seg  p = Seg   (Pos p) (Pos p) deriving (Show, Read, Generic, NFData)

instance (Fractional p, Eq p) => Eq (Line p) where
    l1 == l2 = case lineIntersectionT l1 l2 of
                LTLine  -> True
                _       -> False

instance (Fractional p, Ord p, Eq p) => Eq (Ray p) where
    (Ray p1 d1) == (Ray p2 d2) = p1 == p2 && d1 `crossZ` d2 == 0 && d1 `dot` d2 > 0

instance Eq p => Eq (Seg p) where
    (Seg a1 a2) == (Seg b1 b2) = (a1 == b1 && a2 == b2) || (a1 == b2 && a2 == b1)

instance Arbitrary p => Arbitrary (V2 p) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (V2 x y)

instance Arbitrary p => Arbitrary (Line p) where
    arbitrary = do
        p <- arbitrary
        d <- arbitrary
        return (Line p d)

instance Arbitrary p => Arbitrary (Ray p) where
    arbitrary = do
        p <- arbitrary
        d <- arbitrary
        return (Ray p d)

instance Arbitrary p => Arbitrary (Seg p) where
    arbitrary = do
        p1 <- arbitrary
        p2 <- arbitrary
        return (Seg p1 p2)


data LineIntersectT p
    = LTLine
    | LTPoint p p
    | LTNothing
    deriving (Show, Read, Eq, Generic, NFData)

data LineIntersect p
    = LLine (Line p)
    | LPoint (Pos p)
    | LNothing
    deriving (Show, Read, Eq, Generic, NFData)

data LineRayIntersect p
    = LRRay (Ray p)
    | LRPoint (Pos p)
    | LRNothing
    deriving (Show, Read, Eq, Generic, NFData)

data LineSegIntersect p
    = LSSeg (Seg p)
    | LSPoint (Pos p)
    | LSNothing
    deriving (Show, Read, Eq, Generic, NFData)

data SegIntersect p
    = SSeg (Seg p)
    | SPoint (Pos p)
    | SNothing
    deriving (Show, Read, Eq, Generic, NFData)

data SegRayIntersect p
    = SRSeg (Seg p)
    | SRPoint (Pos p)
    | SRNothing
    deriving (Show, Read, Eq, Generic, NFData)

crossZ :: Num a => V2 a -> V2 a -> a
crossZ (V2 x1 y1) (V2 x2 y2) = (x1 * y2) - (y1 * x2)


lineIntersectionT   :: (Fractional a, Eq a)
                    => Line a           -- ^ First line
                    -> Line a           -- ^ Second line
                    -> LineIntersectT a     -- ^ Line intersection stuff
lineIntersectionT (Line p1 d1) (Line p2 d2)
    = if d2Xd1 == 0
        then if p21Xd2 == 0
            then LTLine
            else LTNothing
        else LTPoint t1 t2
    where
        p21 = p1 - p2

        d2Xd1 = d2 `crossZ` d1
        p21Xd2 = p21 `crossZ` d2

        t1 = p21Xd2 / d2Xd1
        t2 = (p21 `crossZ` d1)  / d2Xd1

lineIntersection    :: (Num a, Fractional a, Eq a)
                    => Line a
                    -> Line a
                    -> LineIntersect a
lineIntersection l1@(Line p1 d1) l2 =
    case lineIntersectionT l1 l2 of
        LTLine         -> LLine l1
        LTPoint t1 _   -> LPoint (p1 + (d1 ^* t1))
        LTNothing      -> LNothing

lineRayIntersection :: (Num a, Fractional a, Ord a, Eq a)
                    => Line a
                    -> Ray a
                    -> LineRayIntersect a
lineRayIntersection line ray@(Ray rayP rayD) =
    case lineIntersectionT line (Line rayP rayD) of
        LTLine         -> LRRay ray
        LTPoint _ rayT -> if 0 <= rayT
                            then LRPoint (rayP + (rayD ^* rayT))
                            else LRNothing
        LTNothing      -> LRNothing

lineSegIntersection :: (Num a, Fractional a, Ord a, Eq a)
                    => Line a
                    -> Seg a
                    -> LineSegIntersect a
lineSegIntersection line seg@(Seg p1 p2) = 
    case lineIntersectionT line (Line p1 p12) of
        LTLine         -> LSSeg seg
        LTPoint _ segT -> if 0 <= segT && segT <= 1
                            then LSPoint (p1 + (p12 ^* segT))
                            else LSNothing
        LTNothing      -> LSNothing
    where
        p12 = p2 - p1

data AorB = A | B deriving (Show, Eq)

segIntersection :: forall a. (Num a, Fractional a, Ord a, Eq a)
                => Seg a
                -> Seg a
                -> SegIntersect a
segIntersection segA@(Seg a1 a2) segB@(Seg b1 b2) =
    case lineIntersectionT (Line a1 a12) (Line b1 b12) of
        LTLine         -> if areIntersecting
                            then if p1 == p2
                                then SPoint p1
                                else SSeg (Seg p1 p2)
                            else SNothing
                        where
                            -- order points along the line
                            orderedPoints :: [(AorB, Pos a)]
                            orderedPoints@((s0, _) : (s1, p1) : (_, p2) : _) = sortOn (dot a12 . snd) [(A, a1), (B, b1), (A, a2), (B, b2)]
                            areIntersecting = s0 /= s1

        LTPoint aT bT -> if 0 <= aT && aT <= 1 && 0 <= bT && bT <= 1
                            then SPoint (a1 + (a12 ^* aT))
                            else SNothing
        LTNothing      -> SNothing
    where
        a12 = a2 - a1
        b12 = b2 - b1

segRayIntersection :: (Num a, Fractional a, Ord a, Eq a)
                   => Seg a
                   -> Ray a
                   -> SegRayIntersect a
segRayIntersection seg (Ray rayP rayD) =
    case lineSegIntersection (Line rayP rayD) seg of
        LSSeg (Seg a b) ->
            if (a - rayP) `dot` rayD < 0
            then
                if (b - rayP) `dot` rayD < 0
                then SRNothing
                else
                    if rayP == b
                    then SRPoint b
                    else SRSeg (Seg rayP b)
            else
                if (b - rayP) `dot` rayD < 0
                then
                    if rayP == a
                    then SRPoint a
                    else SRSeg (Seg rayP a)
                else SRSeg seg

        LSPoint p ->
            if (p - rayP) `dot` rayD < 0
            then SRNothing
            else SRPoint p

        LSNothing -> SRNothing


-- -- Some Geometry
-- data AABB = AABB Pos a Pos a

-- aabb :: [Pos a] -> AABB
-- aabb []     = error "Trying to calculate AABB of an empty set."
-- aabb (p:ps) = foldl accumAABB (AABB p p) ps
--     where
--         accumAABB (AABB (V2 minX minY) (V2 maxX maxY)) (V2 x y)
--             = AABB
--                 (V2 (min minX x) (min minY y))
--                 (V2 (max maxX x) (max maxY y))



-- epsilon = 1e-10

-- data ClipPos a = In Pos a | Out Pos a

-- clipLinePath :: Pos a         -- ^ clipping line point
--              -> Vec a         -- ^ clipping line dir (right of this dir will be clipped)
--              -> [Pos a]       -- ^ path to clip
--              -> [Pos a]       -- ^ clipped path
-- clipLinePath _ _ [] = []
-- clipLinePath lP lDir path = clippedPath
--     where

--         inOutPath = map toClipPos a path

--         edges = zip inOutPath (tail inOutPath)

--         clippedPath = case head inOutPath of
--             In  startP -> startP : clippedPath'
--             Out _      -> clippedPath'

--         clippedPath' = concatMap edgeContribution edges

--         edgeContribution :: (ClipPos a, ClipPos a) -> [Pos a]
--         edgeContribution ( In _,  In b)   = [b]
--         edgeContribution (Out _, Out _)   = []
--         edgeContribution ( In a, Out b)   = [fromJust $ lineIntersection lP lDir a (b - a)]
--         edgeContribution (Out a,  In b)   = [fromJust $ lineIntersection lP lDir a (b - a), b]

--         toClipPos a :: Pos a -> ClipPos a
--         toClipPos a p = if lDir `crossZ` (p - lP) > 0
--             then In p
--             else Out p


-- clipConcavePolygonPath :: [Pos a]    -- ^ clipping concave polygon
--                        -> [Pos a]    -- ^ path to clip
--                        -> [Pos a]    -- ^ clipped line
-- clipConcavePolygonPath clipPolygon path = foldl (\p (a, ab) -> clipLinePath a ab p) path clipLines
--     where
--         clipLines = map (\(a, b) -> (a, (b - a))) $ zip clipPolygon (tail $ cycle clipPolygon)



