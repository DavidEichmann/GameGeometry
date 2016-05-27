{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Geometry.Geometry (

          Line
        , Ray
        , Seg
        , Polygon

        , pattern Line
        , pattern Ray
        , pattern Seg
        , pattern Polygon

        , line
        , line'

        , ray
        , ray'

        , seg
        , seg'

        , polygon

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

        , pointSegIntersection
        , pointLineIntersectionT

        , pointInside

        , HasSeg (..)
        , Lineable (..)

    ) where

import           Data.List
import           Data.Maybe      (fromMaybe)
import           Linear          hiding (rotate, vector)
import           Utils

import           Control.DeepSeq
import           GHC.Generics    (Generic)
import           Test.QuickCheck

data Line    p = Line'    (Pos p) (Vec p) deriving (Show, Read, Generic, NFData)
data Ray     p = Ray'     (Pos p) (Vec p) deriving (Show, Read, Generic, NFData)
data Seg     p = Seg'     (Pos p) (Pos p) deriving (Show, Read, Generic, NFData)
data Polygon p = Polygon' [Pos p]         deriving (Show, Read, Generic, NFData)
-- data Path p

pattern Line    p d <- Line'    p d
pattern Ray     p d <- Ray'     p d
pattern Seg     p d <- Seg'     p d
pattern Polygon ps  <- Polygon' ps

line :: (Eq p, Num p) => Pos p -> Vec p -> Maybe (Line p)
line p d
    | d == zero = Nothing
    | otherwise = Just $ Line' p d

line' :: (Eq p, Num p) => Pos p -> Vec p -> Line p
line' p = fromMaybe (error "Cannot instantiate Line with a direction vector of zero.") . line p

ray :: (Eq p, Num p) => Pos p -> Vec p -> Maybe (Ray p)
ray p d
    | d == zero = Nothing
    | otherwise = Just $ Ray' p d

ray' :: (Eq p, Num p) => Pos p -> Vec p -> Ray p
ray' p = fromMaybe (error "Cannot instantiate Ray with a direction vector of zero.") . ray p

seg :: (Eq p, Num p) => Pos p -> Pos p -> Maybe (Seg p)
seg a b
    | a == b    = Nothing
    | otherwise = Just $ Seg' a b

seg' :: (Eq p, Num p) => Pos p -> Pos p -> Seg p
seg' p = fromMaybe (error "Cannot instantiate Seg with two equal points.") . seg p

polygon :: Eq p => [Pos p] -> Polygon p
polygon = Polygon' . map head . group

instance (Fractional p, Eq p) => Eq (Line p) where
    l1 == l2 = case lineIntersectionT l1 l2 of
                LTLine  -> True
                _       -> False

instance (Fractional p, Ord p, Eq p) => Eq (Ray p) where
    (Ray p1 d1) == (Ray p2 d2) = p1 == p2 && d1 `crossZ` d2 == 0 && d1 `dot` d2 > 0

instance Eq p => Eq (Seg p) where
    (Seg a1 a2) == (Seg b1 b2) = (a1 == b1 && a2 == b2) || (a1 == b2 && a2 == b1)

instance Eq p => Eq (Polygon p) where
    -- Polygons are equal when the list of points or rotated or even reversed
    (Polygon a) == (Polygon b) = elem a $ rotations b ++ (rotations . reverse) b


instance Arbitrary p => Arbitrary (V2 p) where
    arbitrary = V2 <$> arbitrary <*> arbitrary

instance (Ord p, Num p, Arbitrary p) => Arbitrary (Line p) where
    arbitrary = do
        p <- arbitrary
        NonZero d <- arbitrary
        return (Line' p d)

instance (Ord p, Num p, Arbitrary p) => Arbitrary (Ray p) where
    arbitrary = do
        p <- arbitrary
        NonZero d <- arbitrary
        return (Ray' p d)

instance (Ord p, Num p, Arbitrary p) => Arbitrary (Seg p) where
    arbitrary = do
        p1 <- arbitrary
        p2 <- arbitrary `suchThat` (/= p1)
        return (Seg' p1 p2)

instance (Eq p, Arbitrary p) => Arbitrary (Polygon p) where
    arbitrary = do
        l  <- arbitrary
        ps <- vector l
        return (polygon ps)

class HasSeg a c where
    getSeg :: a -> Seg c
    setSeg :: Seg c -> a -> a

instance HasSeg (Seg a) a where
    getSeg = id
    setSeg = const

instance HasSeg (l, Seg a) a where
    getSeg = snd
    setSeg newSeg (label, _) = (label, newSeg)

class Lineable a c where
    toLine :: a -> Line c

instance (Eq c, Num c) =>  Lineable (Seg c) c where
    toLine (Seg a b) = line' a (b - a)

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

lineIntersectionT   :: (Fractional a, Eq a)
                    => Line a           -- ^ First line
                    -> Line a           -- ^ Second line
                    -> LineIntersectT a -- ^ Line intersection stuff
lineIntersectionT l1@(Line p1 d1) l2@(Line p2 d2)
    = if d2Xd1 == 0
        then
            if p21Xd2 == 0
            then LTLine
            else LTNothing
        else LTPoint t1 t2
    where
        p21 = p1 - p2

        d2Xd1 = d2 `crossZ` d1
        p21Xd2 = p21 `crossZ` d2

        t1 = p21Xd2 / d2Xd1
        t2 = (p21 `crossZ` d1) / d2Xd1

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
    case lineIntersectionT line (Line' rayP rayD) of
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
    case lineIntersectionT line (Line' p1 p12) of
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
segIntersection segA@(Seg a1 a2) segB@(Seg b1 b2)
    | a1 == a2  = maybe SNothing SPoint $ pointSegIntersection a1 segB
    | b1 == b2  = maybe SNothing SPoint $ pointSegIntersection b1 segA
    | otherwise = case lineIntersectionT (Line' a1 a12) (Line' b1 b12) of
        LTLine        -> if areIntersecting
                            then if p1 == p2
                                then SPoint p1
                                else SSeg (Seg' p1 p2)
                            else SNothing
                        where
                            -- order points along the line
                            orderedPoints :: [(AorB, Pos a)]
                            orderedPoints@((s0, _) : (s1, p1) : (_, p2) : _) = sortOn (dot a12 . snd) [(A, a1), (B, b1), (A, a2), (B, b2)]
                            areIntersecting = s0 /= s1

        LTPoint aT bT -> if 0 <= aT && aT <= 1 && 0 <= bT && bT <= 1
                            then SPoint (a1 + (a12 ^* aT))
                            else SNothing
        LTNothing     -> SNothing
    where
        a12 = a2 - a1
        b12 = b2 - b1

segRayIntersection :: (Num a, Fractional a, Ord a, Eq a)
                   => Seg a
                   -> Ray a
                   -> SegRayIntersect a
segRayIntersection seg (Ray rayP rayD) =
    case lineSegIntersection (Line' rayP rayD) seg of
        LSSeg (Seg a b) ->
            if (a - rayP) `dot` rayD < 0
            then
                if (b - rayP) `dot` rayD < 0
                then SRNothing
                else
                    if rayP == b
                    then SRPoint b
                    else SRSeg (Seg' rayP b)
            else
                if (b - rayP) `dot` rayD < 0
                then
                    if rayP == a
                    then SRPoint a
                    else SRSeg (Seg' rayP a)
                else SRSeg seg

        LSPoint p ->
            if (p - rayP) `dot` rayD < 0
            then SRNothing
            else SRPoint p

        LSNothing -> SRNothing

pointSegIntersection :: (Eq p, Ord p, Num p) => Pos p -> Seg p -> Maybe (Pos p)
pointSegIntersection p (Seg a b)
    | p == a ||
      p == b ||
      (pa `crossZ` pb == 0 && pa `dot` pb < 0)
                = Just p
    | otherwise = Nothing
    where
        pa = a - p
        pb = b - p

pointLineIntersectionT :: (Eq p, Fractional p) => Pos p -> Line p -> Maybe p
pointLineIntersectionT p (Line x d)
    | p == x ||
      xp `crossZ` d == 0
                = Just $ xp `dot` d / quadrance d
    | otherwise = Nothing
    where
        xp = p - x

pointInside :: forall p. (Fractional p, Ord p) => Pos p -> Polygon p -> Bool
pointInside (V2 x y) (Polygon' shape) = foldl' (/=) False . map intersectsEdge . zip shape $ rotate 1 shape
    where
        intersectsEdge :: (Pos p, Pos p) -> Bool
        intersectsEdge (V2 ax ay, V2 bx by)
            = (ay > y) /= (by > y)
            && x < (bx - ax) * (y - ay) / (by - ay) + ax

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



