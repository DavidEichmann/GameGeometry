{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Geometry.Shadows (
        shadowFronts

        , ShadowFront(..)
        , initialMergeState
        , MergeState
    ) where

import Utils
import Geometry.Geometry
import Geometry.Angles
import GHC.Exts
import Safe
import Data.Maybe (catMaybes, mapMaybe, fromJust, isJust, fromMaybe, maybeToList)
import Data.List (partition, zipWith3, groupBy, sortOn, find)
import Data.List.Split
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Linear
import qualified Data.Set as S

import Debug.Trace


type ShadowFrontSeg label p = (label, Seg p) -- (label, spreadX2, squareDist from focal point, point)
-- list or segments ordered by SpreadX2 and split by empty space. Ther is no "visual" overlap about the focal point
-- elements always have atleadt 2 elements (a single segment)
-- first element is the START of a segment after SpreadX2 == 0 (i.e. the point after )
data ShadowFront label p = ShadowFront [ShadowFrontSeg label p]

toLabeledSegs :: ShadowFront label p -> [(label, Seg p)]
toLabeledSegs = undefined

empty :: ShadowFront label p
empty = ShadowFront []

merge :: ShadowFront label p -> ShadowFront label p -> ShadowFront label p
merge = undefined

filterByFront :: ShadowFront label p -> ShadowFront label p -> ShadowFront label p
filterByFront = undefined

toShadowFront :: [(label, Seg p)] -> ShadowFront label p    -- this is O(n log(n)) by using divide and conquer (and merge)
toShadowFront = undefined
-- TODO: make sure to filter out colinaer segs w.r.t the focal point

-- Helpers
flipNature :: SegNature -> SegNature
flipNature Near = Far
flipNature Far  = Near
data SegNature = Near | Far
    deriving (Show)

data StartSegBreak label p = StartSegBreak SegNature (ShadowFrontSeg label p)
    deriving (Show)

data MergeState label p
    = MergeState
        (ShadowFrontSeg label p) [ShadowFrontSeg label p]           -- ^ current near seg and future segs
        (Maybe (ShadowFrontSeg label p)) [ShadowFrontSeg label p]   -- ^ current far seg and future segs
    deriving (Show)

initialMergeState :: forall label p. (Fractional p, Ord p)
     => Pos p                                           -- ^ focal point
     -> ShadowFront label p                             -- ^ first front
     -> ShadowFront label p                             -- ^ second front
     -> (MergeState label p, Maybe (StartSegBreak label p))       -- ^ initial merge state and possible broken segment
-- assume that the inputs are non-empty
initialMergeState focalPoint (ShadowFront allAs@((a@(al, Seg a1 a2)) : as)) (ShadowFront allBs@((b@(bl, Seg b1 b2)) : bs))
    = if aSpread == bSpread
        then (MergeState a as (Just b) bs, Nothing)         -- is this needed?
        else if brokenFirstFrontStartsOnNonEmptySpace
            then if brokenFirstFrontP `qd` focalPoint > secondFrontPoint `qd` focalPoint
                then (MergeState (head secondFront)      (tail secondFront)      (Just $ head brokenFirstFront) (tail brokenFirstFront)
                        , StartSegBreak Far  <$> brokenFirstFrontSegMay)
                else (MergeState (head brokenFirstFront) (tail brokenFirstFront) (Just $ head secondFront)      (tail secondFront)
                        , StartSegBreak Near <$> brokenFirstFrontSegMay)
            else (MergeState (head secondFront) (tail secondFront) Nothing brokenFirstFront, StartSegBreak Far <$> brokenFirstFrontSegMay)
    where

        aSpread = spreadX2 a1
        bSpread = spreadX2 b1

        (firstFront, secondFront, secondFrontSpr, secondFrontPoint) = if aSpread > bSpread
            then (allBs, allAs, aSpread, a1)
            else (allAs, allBs, bSpread, b1)

        -- rotate the first front till the second front's point is on or before the rotated second fronts first segment.
        -- If no such rotaton exists, then the second spred must be at the end of all the first front's segments (in an empty space).
        (brokenFirstFront@((_, Seg brokenFirstFrontP _):_), brokenFirstFrontSegMay, brokenFirstFrontStartsOnNonEmptySpace) = breakAt secondFrontSpr secondFrontPoint firstFront

        breakAt :: Spr p -> Pos p -> [ShadowFrontSeg label p] -> ([ShadowFrontSeg label p], Maybe (ShadowFrontSeg label p), Bool)
        breakAt spr sprP (xs) = fromMaybe (xs, Nothing, False) $ headMay $ mapMaybe isGoodStart (rotations xs)
            where
                isGoodStart :: [ShadowFrontSeg label p] -> Maybe ([ShadowFrontSeg label p], Maybe (ShadowFrontSeg label p), Bool)
                isGoodStart frontSegs@((l, Seg c d):rest)
                    | sprC > spr                = Just (frontSegs, Nothing, False)
                    | isOnSpr spr sprC sprD   = case lineIntersection (line' focalPoint sprP) (line' c (d - c)) of
                                                            LPoint p    -> let breakSegMay = do s <- seg c p; return (l, s)
                                                                            in Just (((maybeToList $ do s <- seg p d; return (l, s)) ++ rest ++ (maybeToList breakSegMay)), breakSegMay, True)
                                                            -- anything else just don't break... This case is mathematically impossible, but may happen due to rounding errors.
                                                            _           -> Just (frontSegs, Nothing, True)
                                                    
                    | otherwise                 = Nothing
                    where
                        sprC = spreadX2 c
                        sprD = spreadX2 d






-- TODO how do you resolve overlapping fronts?!?!?!?!


-- | Given a bunch of (possibly annotated) segments and a focal point, this will calculate the "shaddow fronts".
-- This is all the connected immediatelly visible segments from the focal point (X). These segments may be cut:
-- 
--    ---A---
--    |     |                          
--    B     D                   |      
--    |     |                   |     
--    ---C---                   |       
--                        |     |       
--              X         E     F       
--                        |     |       
--                              |       
--                              |       
--                              |     
-- 
-- Goes to:
--
--          |                          
--          D                   |      
--          |                   F     
--    ---C---                   |       
--                        |             
--              X         E             
--                        |             
--                              |       
--                              F       
--                              |       
--
-- Grouped by connected runs e.g. in the above case: [[D,C],[F,E,F]]
shadowFronts :: forall ss p. (Ord p, Num p, Fractional p, HasSeg ss p

                    , Show ss, Show p)

                => Pos p          -- ^ focal point
                -> [ss]           -- ^ all the segments
                -> Maybe [Seg p]  -- ^ Filter segments
                -> [[ss]]         -- ^ front segments
shadowFronts focalPoint hasSegs filterSegs = fronts
    where

        ssAll :: Vector ss
        ssAll = fromList hasSegs

        segAll :: Vector (Seg p)
        segAll = V.map getSeg ssAll

        ssAt :: SegIx -> ss
        ssAt (SegIx ix) = ssAll ! ix

        segAt :: SegIx -> Seg p
        segAt (SegIx ix) = segAll ! ix

        -- ## NOTE here the colinear points are filtered out ##
        -- Get all the start/end point events
        segToEvents :: SegIx -> [(EventType, Pos p, SegIx)]
        segToEvents segIx = case (a - focalPoint) `crossZ` (b - a) of
                                    0 -> []
                                    z 
                                        | z > 0     -> [(Start, a, segIx), (End,   b, segIx)]
                                        | otherwise -> [(End,   a, segIx), (Start, b, segIx)]
            where
                Seg a b = segAt segIx

        unorderedEvents :: [(EventType, Pos p, SegIx)]
        unorderedEvents = concatMap segToEvents [0..SegIx $ (V.length segAll) - 1]

        -- Sort the events by Angle and store as [(start points and Segs, end points and segs)]
        eventsSortedBySpread :: [(p, ([(Pos p, SegIx)], [(Pos p, SegIx)]))]
        eventsSortedBySpread = sortOn fst [(the pointSpread, ([(p,i) | (Start,p,i) <- x], [(p,i) | (End,p,i) <- x]))
                    | x@(_, pos, segIx) <- unorderedEvents
                    , let pointSpread = unSpr $ spreadX2 (pos - focalPoint)
                    , then group by pointSpread using groupWith]

        -- Sort the events by Angle and store as [(start points and Segs, end points and segs)]
        events :: [([(Pos p, SegIx)], [(Pos p, SegIx)])]
        events = map snd $ eventsSortedBySpread

        eventsSegIx :: [(S.Set SegIx, S.Set SegIx)]
        eventsSegIx = map (\(new, old) -> (S.fromList (map snd new), S.fromList (map snd old))) events

        -- Go through all the events once the get the hot edges just before the first event.
        initialHotEdges :: S.Set SegIx
        initialHotEdges = foldl
                            (\hotEdges (new, old) -> (hotEdges `S.union` new) S.\\ old)
                            S.empty
                            eventsSegIx

        -- Now that we have the initial hot edges, scan through the events again to get the hot edges for each wedge.
        -- NOTE: the number of wedges == number of events
        -- NOTE: Segs that end at an event are NOT included in the hot edges for the wedge.
        wedgeHotEdges :: [S.Set SegIx]
        wedgeHotEdges = tail $ scanl                -- tail is safe to do as the first element of scanl is initialHotEdges
                            (\hotEdges (new, old) -> (hotEdges `S.union` new) S.\\ old)
                            initialHotEdges
                            eventsSegIx

        -- There may be intersections in the wedges, so break down each wedge at the intersection poitns.
        -- Since there are no start/end points within the wedge, intersection can be detected by the ordering
        -- of segments' distances from the focal points at the start vs. end of the weged:
        --
        --        Wedge End
        --                       ____ A ____________                                        
        --             _B____C__/                                                           
        --          __/                                                                    
        --        /                                                                         
        --     F =                                               ==> start: [A, B, C]                       
        --         \ __                                              end:   [B, C, A]                       
        --              |_______A_                                                          
        --                         \______B___                   ==> C intersects A as their relative positions swap
        --        Wedge Start                 \_C_____               B intersects A
        --                                                           A intersects B and C

        -- ### NOTE ###
        -- For now we only care about the closest (to the focal point) segments (the shadow front) so we only need
        -- to traverse the closest segments. In the example above:
        -- 1. A is closest to F at the start of the wedge
        -- 2. A intersects B and C
        -- 3. Check which intersection happens first (in this case A intersects B before C).
        -- 4. Break the wedge at the intersection of A and B
        -- 5. The subwedge between the Start and the new break is correct
        -- 6. Recurse to the subwedge between the new break and the end

        wedgeRays :: [Ray p]  -- Representative point on each wedge ray
        wedgeRays =
            map
                (ray' focalPoint . (\repPoint -> repPoint - focalPoint)) -- we use ray' because we already filter out points that are on the focal points
                [head $ (map fst starts) ++ (map fst ends) | (starts, ends) <- events] -- head is possible since all events must be non-empty

        wedgeStartEndRayCasts :: [([(Pos p, SegIx)], [(Pos p, SegIx)])]
        wedgeStartEndRayCasts =
            zipWith3
                (\rayStart rayEnd hotEdges ->
                    let segs = S.toList hotEdges
                    in (doRayCastWithLines rayStart segs, doRayCastWithLines rayEnd segs))
                wedgeRays
                (Utils.rotate 1 wedgeRays)
                wedgeHotEdges
                where
                    -- TODO improve ray casting code to accept lines
                    -- NOTE, we convert to lines in order to account for rounding errors
                    doRayCastWithLines :: Ray p -> [SegIx] -> [(Pos p, SegIx)]
                    doRayCastWithLines ray@(Ray f _) segIxs = 
                        [(p, segIx)     | (line, segIx) <- zip lines segIxs
                                        -- , let LRPoint p = traceShow ("initialHotEdges", initialHotEdges, ray, line, lineRayIntersection line ray) $ lineRayIntersection line ray  -- as the segs must not be colinear with the focalpoint, all intersections must be points
                                        -- TODO: I managed to get this to fail once (using Floats): the result was not an LRPoint
                                        , let LRPoint p = lineRayIntersection line ray  -- as the segs must not be colinear with the focalpoint, all intersections must be points
                                        , then sortWith by quadrance (p - f)]
                        where
                            lines :: [Line p]
                            lines = map (toLine . segAt) segIxs


        dividedWedges :: [[(Seg p, SegIx)]]
        dividedWedges = map (\case
            ([], _)             -> []
            ((front:_), end)    -> divideWedge front end) wedgeStartEndRayCasts
            where
                divideWedge :: (Pos p, SegIx)       -- ^ current front point
                            -> [(Pos p, SegIx)]     -- ^ end raycast
                            -> [(Seg p, SegIx)]     -- ^ Front segments
                divideWedge front endRayCast = catMaybes $ divideWedge' front
                    where
                        endRayCastSegIxs = map snd endRayCast

                        divideWedge' :: (Pos p, SegIx) -> [Maybe (Seg p, SegIx)]
                        divideWedge' (frontP, frontSegIx) = currentFrontSegMay : maybe [] divideWedge' nextPSegIx
                            where
                                intersectingSegIxs = takeWhile (/= frontSegIx) endRayCastSegIxs
                                intersectionExists = isJust nextPSegIx

                                -- current edge (may be nothing if the next point is equal to the current point
                                --      this can happen e.g. when 2 segments start at the same point on the start ray)
                                currentFrontSegMay :: Maybe (Seg p, SegIx)
                                currentFrontSegMay = do
                                    nonPointSegment <- seg frontP nextP
                                    return (nonPointSegment, frontSegIx)

                                nextP = maybe
                                            (fst . fromJust . find ((==frontSegIx) . snd) $ endRayCast) -- current seg intersect with end ray
                                            fst nextPSegIx


                                nextPSegIx = headMay [(p, segIx) | segIx <- intersectingSegIxs
                                                        , let i = lineIntersection (toLine $ segAt segIx) (toLine $ segAt frontSegIx)
                                                        , LPoint p <- [i]
                                                        , then sortWith by (qd frontP p)]

        -- Convert back to HasSegments
        fronts :: [[ss]]
        fronts
            = map (map toSS . clean)
            . filter (not . null)
            . wrap
            . map concat
            . groupBy (\a b -> null a && null b)
            . map concat
            . splitOn [[]]
            $ dividedWedges
            where
                toSS :: (Seg p, SegIx) -> ss
                toSS (s, segIx) = setSeg s (ssAt segIx)

                clean :: [(Seg p, SegIx)] -> [(Seg p, SegIx)]
                clean fs
                    = mapMaybe (\g -> let
                                    Seg segStartP _ = fst $ head g
                                    Seg _ segEndP   = fst $ last g
                                 in do s <- seg segStartP segEndP; return (s, snd $ head g))
                    . groupBy (\(_, aIx) (_, bIx) -> aIx == bIx)
                    $ fs

                wrap :: Show a =>  [[a]] -> [[a]]
                wrap [] = []
                wrap [a] = [a]
                wrap orig@(x:xs) = ((last orig) ++ x) : init xs


newtype SegIx = SegIx { unSegIx :: Int } deriving (Eq, Show, Enum, Ord, Num)
data EventType = Start | End
    deriving (Eq, Show, Ord)
