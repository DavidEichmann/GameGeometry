{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Geometry.Shadows (

          ShadowFront

        , hasSegsToShadowFront
        , shadowFrontToHasSegs

        , toShadowFront
        , toLabeledSegs

        , merge
        , filterMerge

        , SegIx(..)
        -- , initialMergeState
        -- , MergeState
    ) where

import Utils
import Geometry.Geometry
import Geometry.Angles
import GHC.Exts
import Safe
import Data.Maybe (catMaybes, mapMaybe, fromJust, isJust, fromMaybe, maybeToList)
import Data.List (partition, zipWith3, groupBy, sortOn, find)
import Data.List.Split
import Data.Function (on)
import Data.Vector (Vector, (!))
import Control.Monad (mplus)
import qualified Data.Vector as V
import Linear hiding (trace)
import qualified Data.Set as S

import Debug.Trace


-- list or segments ordered by SpreadX2 and split by empty space. Ther is no "visual" overlap about the focal point
-- elements always have atleadt 2 elements (a single segment)
-- first element is the START of a segment after SpreadX2 == 0 (i.e. the point after )
-- a segment represented by a spread(P) range and a line
newtype ShadowFront l p = ShadowFront { unShadowFront :: [WLine l p] }
    deriving (Show)

data WLine l p = WLine l ((p,Pos p), (p, Pos p)) (Line p)
    deriving (Show)

toLabeledSegs :: (Eq p, Ord p, Fractional p) => Pos p -> ShadowFront l p -> [(l, Seg p)]
toLabeledSegs focalPoint (ShadowFront wLines) = mapMaybe (wlineToLabelSeg focalPoint) wLines

wlineToLabelSeg :: (Eq p, Ord p, Fractional p) => Pos p -> WLine l p -> Maybe (l, Seg p)
wlineToLabelSeg focalPoint (WLine l ((aS, aP), (bS, bP)) cLine) = do
    -- Single segment, so try and find points of intersection with wedge rays
    aRay <- ray focalPoint (aP - focalPoint)
    bRay <- ray focalPoint (bP - focalPoint)
    pointA <- case lineRayIntersection cLine aRay of LRPoint p -> Just p; _ -> Nothing
    pointB <- case lineRayIntersection cLine bRay of LRPoint p -> Just p; _ -> Nothing
    s <- seg pointA pointB
    return (l, s)








-- TODO: cross product may indicate the incorrect direction when the points are close.... instead, try to break on the x+ ray, then sort and flip according to spreads only!



-- %%% make this part of the toShadowFront function
toWLines :: (Ord p, Fractional p)
         => Pos p
         -> (l, Seg p)
         -> ShadowFront l p
toWLines focalPoint (l, s@(Seg aInit bInit))
    = if isColinear
        then empty
        else if aS > bS
            then case lineIntersection directedLine (line' focalPoint (V2 1 0)) of
                LPoint mid -> ShadowFront $ catMaybes [
                                    WLine l (( 0, mid), (bS,   b)) <$> line mid (  b - mid),
                                    WLine l ((aS,   a), ( 4, mid)) <$> line   a (mid -   a)
                                ]
                _          -> empty
            else ShadowFront [WLine l ((aS, a), (bS, b)) directedLine]

    where
        aInitV              = aInit - focalPoint
        bInitV              = bInit - focalPoint
        aInitS              = unSpr $ spreadX2 aInitV
        bInitS              = unSpr $ spreadX2 bInitV

        isColinear  = aInitV `crossZ` bInitV == 0

        (directed@(Seg a b), aS, bS)
                    = if aInitV `crossZ` (bInit - aInit) > 0
                                then (s, aInitS, bInitS)
                                else (seg' bInit aInit, bInitS, aInitS)
        directedLine = toLine directed

empty :: ShadowFront l p
empty = ShadowFront []

toShadowFront :: forall l p. (Fractional p, Ord p, Show p, Show l)
    => Pos p
    -> Maybe [(l, Seg p)]
    -> [(l, Seg p)]
    -> ShadowFront l p    -- this is O(n log(n)) (???are you sure???) by using divide and conquer (and merge)
toShadowFront focalPoint filterLabeledSegsMay labeledSegs
    -- divide, conquer, merge
    = toShadowFront'
    -- filter all  individual shadow fronts by the filter ([ShadowFront])
    . map applyFilter
    -- Convert single segs to shadow fronts ([ShadowFront])
    . map (toWLines focalPoint)
    $ labeledSegs
    where
        applyFilter :: ShadowFront l p -> ShadowFront l p
        applyFilter = case filterLabeledSegsMay of
            Nothing   -> id
            Just flts -> filterMerge focalPoint (filterWLines flts) -- %%%  call toShadowFront for flts instead of what is being done here

        -- %%% see last comment above, this us just a special case of toShadowFront
        filterWLines :: [(l, Seg p)] -> ShadowFront l p
        filterWLines = toShadowFront' . map (toWLines focalPoint)

        toShadowFront' :: [ShadowFront l p] -> ShadowFront l p
        toShadowFront' []       = ShadowFront []
        toShadowFront' [f]      = f
        toShadowFront' fs       = merge focalPoint (toShadowFront' fsA) (toShadowFront' fsB)  -- %%% instead of having a toShadowFront', can you do this at the top level?
            where
                (fsA, fsB) = splitAt (length fs `div` 2) fs

-- TODO merge spreads across the positive x ray
joinSegs :: ShadowFront l p -> ShadowFront l p
joinSegs = id

filterMerge :: forall p l. (Ord p, Fractional p, Num p, Show p, Show l)
      => Pos p
      -> ShadowFront l p
      -> ShadowFront l p
      -> ShadowFront l p
filterMerge focalPoint (ShadowFront filterSF) (ShadowFront sf)
    = joinSegs
    . ShadowFront
    . mapMaybe mergeWedge
    $ toWedges filterSF sf
    where
        -- break into wedges
        toWedges :: [WLine l p]
                 -> [WLine l p]
                 -> [(((p, Pos p), (p, Pos p)), ((l, Line p), (l, Line p)))]
                    -- ^ [((start/end spread and point on spreads), either 1 or 2 labeled segments (unaltered))]
        toWedges [] [] = []
        toWedges as [] = [] -- *** map (\c@(WLine cL cSSPs cLine) -> (cSSPs, Left (cL, cLine))) as
        toWedges [] bs = [] -- *** toWedges bs []
        toWedges
            ((a@(WLine aL aSSPs@((aS1,aSP1),(aS2,aSP2)) aLine)) : as)
            -- (a@(aLids@LIDS{lidsSprs=(aS1',aS2)}, aSeg@(Seg a1 a2)):as)
            ((b@(WLine bL bSSPs@((bS1,bSP1),(bS2,bSP2)) bLine)) : bs)
            -- (b@(bLids@LIDS{lidsSprs=(bS1',bS2)}, bSeg@(Seg b1 b2)):bs)
                = case compare aS1 bS1 of
                    -- both start on the same spread
                    EQ -> case compare aS2 bS2 of
                        -- both end at the same spred -> consume both
                        EQ -> (aSSPs, ((aL, aLine), (bL, bLine))) : toWedges as bs
                        -- a ends first -> consume a
                        LT -> (aSSPs, ((aL, aLine), (bL, bLine))) : toWedges as ((WLine bL ((aS2,aSP2),(bS2,bSP2)) bLine):bs)
                        -- b ends first -> consume b
                        GT -> (bSSPs, ((aL, aLine), (bL, bLine))) : toWedges ((WLine aL ((bS2,bSP2),(aS2,aSP2)) aLine):as) bs
                    -- a starts before b
                    LT -> if aS2 <= bS1
                        -- a is entirely before b -> consume a
                        then toWedges as (b:bs) -- ***
                        -- a and b overlap -> consume a up to start of b
                        else toWedges ((WLine aL ((bS1,bSP1),(aS2,aSP2)) aLine):as) (b:bs) -- ***
                    -- b starts before a
                    GT -> if bS2 <= aS1
                        -- b is entirely before a -> consume b
                        then toWedges (a:as) bs -- ***
                        -- b and a overlap -> consume b up to start of a
                        else toWedges (a:as) ((WLine bL ((aS1,aSP1),(bS2,bSP2)) bLine):bs) -- ***

        maybeToList' _ (Just x) = [x]
        maybeToList' msg _ = [] -- error msg

        mergeWedge :: (((p, Pos p), (p, Pos p)), ((l, Line p), (l, Line p)))
                   -> Maybe (WLine l p)
        mergeWedge (((aS, aP), (bS, bP)), ((uL, uLine), (vL, vLine)))
            = do
                -- get wedge rays
                aRay <- ray focalPoint (aP - focalPoint)
                bRay <- ray focalPoint (bP - focalPoint)

                -- Ray cast along the 2 wedge rays against the 2 segments
                case (lineRayIntersection uLine aRay, lineRayIntersection uLine bRay, lineRayIntersection vLine aRay, lineRayIntersection vLine bRay) of
                    
                    -- The case we expect!
                    (LRPoint uaP, LRPoint ubP, LRPoint vaP, LRPoint vbP) ->
                        let
                            -- TODO could retain intersection result instead of recalculating later.
                            full = WLine vL ((aS, aP), (bS, bP)) vLine
                            midPMay  = case lineIntersection uLine vLine of LPoint p -> Just p; _ -> Nothing
                        in case (compare (uaP `qd` focalPoint) (vaP `qd` focalPoint), compare (ubP `qd` focalPoint) (vbP `qd` focalPoint)) of
                            -- Segments are on top of each other... just pick an arbitrary one
                            (EQ, EQ) -> Nothing

                            -- = shape. take the further one
                            (LT, LT) -> Just full
                            (GT, GT) -> Nothing

                            -- > or < shape. Pick the one that is behind on at least one side
                            (EQ, LT) -> Just full
                            (LT, EQ) -> Just full
                            (EQ, GT) -> Nothing
                            (GT, EQ) -> Nothing

                            -- X shape.
                            -- if mid point is invalid, then just use arbitrary full segment
                            (LT, GT) -> do
                                midP <- midPMay
                                let midS = unSpr $ spreadX2 (midP - focalPoint)
                                return $ WLine vL ((aS, aP), (midS, midP)) vLine
                            (GT, LT) ->  do
                                midP <- midPMay
                                let midS = unSpr $ spreadX2 (midP - focalPoint)
                                return $ WLine vL ((midS, midP), (bS, bP)) vLine

                    -- *** one or both segs appear invalid... drop this wedge
                    _ -> Nothing


merge :: forall p l. (Ord p, Fractional p, Num p, Show p, Show l)
      => Pos p
      -> ShadowFront l p
      -> ShadowFront l p
      -> ShadowFront l p
merge focalPoint (ShadowFront as) (ShadowFront bs)
    = joinSegs
    . ShadowFront
    . concatMap mergeWedge
    $ toWedges as bs
    where
        -- break into wedges
        toWedges :: [WLine l p]
                 -> [WLine l p]
                 -> [(((p, Pos p), (p, Pos p)), Either (l, Line p) ((l, Line p), (l, Line p)))]
                    -- ^ [((start/end spread and point on spreads), either 1 or 2 labeled segments (unaltered))]
        toWedges [] [] = []
        toWedges as [] = map (\c@(WLine cL cSSPs cLine) -> (cSSPs, Left (cL, cLine))) as
        toWedges [] bs = toWedges bs []
        toWedges
            ((a@(WLine aL aSSPs@((aS1,aSP1),(aS2,aSP2)) aLine)) : as)
            -- (a@(aLids@LIDS{lidsSprs=(aS1',aS2)}, aSeg@(Seg a1 a2)):as)
            ((b@(WLine bL bSSPs@((bS1,bSP1),(bS2,bSP2)) bLine)) : bs)
            -- (b@(bLids@LIDS{lidsSprs=(bS1',bS2)}, bSeg@(Seg b1 b2)):bs)
                = case compare aS1 bS1 of
                    -- both start on the same spread
                    EQ -> case compare aS2 bS2 of
                        -- both end at the same spred -> consume both
                        EQ -> (aSSPs, Right ((aL, aLine), (bL, bLine))) : toWedges as bs
                        -- a ends first -> consume a
                        LT -> (aSSPs, Right ((aL, aLine), (bL, bLine))) : toWedges as ((WLine bL ((aS2,aSP2),(bS2,bSP2)) bLine):bs)
                        -- b ends first -> consume b
                        GT -> (bSSPs, Right ((aL, aLine), (bL, bLine))) : toWedges ((WLine aL ((bS2,bSP2),(aS2,aSP2)) aLine):as) bs
                    -- a starts before b
                    LT -> if aS2 <= bS1
                        -- a is entirely before b -> consume a
                        then (aSSPs, Left (aL, aLine)) : toWedges as (b:bs)
                        -- a and b overlap -> consume a up to start of b
                        else (((aS1,aSP1),(bS1,bSP1)), Left (aL, aLine)) : toWedges ((WLine aL ((bS1,bSP1),(aS2,aSP2)) aLine):as) (b:bs)
                    -- b starts before a
                    GT -> if bS2 <= aS1
                        -- b is entirely before a -> consume b
                        then (bSSPs, Left (bL, bLine)) : toWedges (a:as) bs
                        -- b and a overlap -> consume b up to start of a
                        else (((bS1,bSP1),(aS1,aSP1)), Left (bL, bLine)) : toWedges (a:as) ((WLine bL ((aS1,aSP1),(bS2,bSP2)) bLine):bs)

        maybeToList' _ (Just x) = [x]
        maybeToList' msg _ = [] -- error msg

        mergeWedge :: (((p, Pos p), (p, Pos p)), Either (l, Line p) ((l, Line p), (l, Line p)))
                   -> [WLine l p]
        mergeWedge (((aS, aP), (bS, bP)), wlines)
            = case wlines of
                -- Single segment, so try and find points of intersection with wedge rays
                Left (cL, cLine) -> [WLine cL ((aS, aP), (bS, bP)) cLine]

                Right ((uL, uLine), (vL, vLine)) -> fromMaybe [] $ do
                    -- get wedge rays
                    aRay <- ray focalPoint (aP - focalPoint)
                    bRay <- ray focalPoint (bP - focalPoint)

                    -- Ray cast along the 2 wedge rays against the 2 segments
                    return $ case (lineRayIntersection uLine aRay, lineRayIntersection uLine bRay, lineRayIntersection vLine aRay, lineRayIntersection vLine bRay) of
                        
                        -- The case we expect!
                        (LRPoint uaP, LRPoint ubP, LRPoint vaP, LRPoint vbP) ->
                            let
                                -- TODO could retain intersection result instead of recalculating later.
                                full1 = WLine uL ((aS, aP), (bS, bP)) uLine
                                full2 = WLine vL ((aS, aP), (bS, bP)) vLine
                                midPMay  = case lineIntersection uLine vLine of LPoint p -> Just p; _ -> Nothing
                                arbMay   = full1
                            in case (compare (uaP `qd` focalPoint) (vaP `qd` focalPoint), compare (ubP `qd` focalPoint) (vbP `qd` focalPoint)) of
                                -- Segments are on top of each other... just pick an arbitrary one
                                (EQ, EQ) -> [arbMay]

                                -- = shape. take the closer one
                                (LT, LT) -> [full1]
                                (GT, GT) -> [full2]

                                -- > or < shape. Pick the one that is in front on at least one side
                                (EQ, LT) -> [full1]
                                (LT, EQ) -> [full1]
                                (EQ, GT) -> [full2]
                                (GT, EQ) -> [full2]

                                -- X shape.
                                -- if mid point is invalid, then just use arbitrary full segment
                                (LT, GT) -> maybe [arbMay] (\midP -> let midS = unSpr $ spreadX2 (midP - focalPoint) in [
                                        WLine uL ((aS, aP), (midS, midP)) uLine,
                                        WLine vL ((midS, midP), (bS, bP)) vLine
                                    ]) midPMay
                                (GT, LT) -> maybe [arbMay] (\midP -> let midS = unSpr $ spreadX2 (midP - focalPoint) in [
                                        WLine vL ((aS, aP), (midS, midP)) vLine,
                                        WLine uL ((midS, midP), (bS, bP)) uLine
                                    ]) midPMay

                        -- one of the segs appears invalid... just use the other seg
                        -- x@(LRPoint uaP, LRPoint ubP, _, _) -> error $ "second (v) is invalid\n((aS, aP), (bS, bP)): " ++ show ((aS, aP), (bS, bP)) ++ "\nfocalPoint: " ++ show focalPoint ++ "\n(uLine, vLine): " ++ show (uLine, vLine) ++ "\nintersections: " ++ show x
                        -- x@(_, _, LRPoint vaP, LRPoint vbP) -> error $ "first (u) is invalid\n((aS, aP), (bS, bP)): " ++ show ((aS, aP), (bS, bP)) ++ "\nfocalPoint: " ++ show focalPoint ++ "\n(uLine, vLine): " ++ show (uLine, vLine) ++ "\nintersections: " ++ show x
                        (LRPoint uaP, LRPoint ubP, _, _) -> [WLine uL ((aS, aP), (bS, bP)) uLine] -- seg uaP ubP
                        (_, _, LRPoint vaP, LRPoint vbP) -> [WLine vL ((aS, aP), (bS, bP)) vLine] -- seg vaP vbP
                        -- both segs appear invalid... drop this wedge
                        _ -> error "2 invalid segments in merge wedge" -- []


filterByFront :: ShadowFront l p -> ShadowFront l p -> ShadowFront l p
filterByFront = undefined


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

shadowFrontToHasSegs :: (Eq p, Ord p, Fractional p, HasSeg ss p) => Pos p -> ShadowFront ss p -> [ss]
shadowFrontToHasSegs focalPoint = map (\(l, s) -> setSeg s l) . toLabeledSegs focalPoint

hasSegsToShadowFront :: (Ord p, Num p, Fractional p, Show p, Show ss, HasSeg ss p)
             => Pos p -> Maybe [ss] -> [ss] -> ShadowFront ss p
hasSegsToShadowFront focalPoint filterHasSegsMay hasSegs
    = toShadowFront focalPoint (map ((undefined,) . getSeg) <$> filterHasSegsMay)
    . map (\hs -> (hs, getSeg hs))
    $ hasSegs


-- shadowFronts' :: forall ss p. (Ord p, Num p, Fractional p, HasSeg ss p

--                     , Show ss, Show p)

--                 => Pos p          -- ^ focal point
--                 -> [ss]           -- ^ all the segments
--                 -> [[ss]]         -- ^ front segments
-- shadowFronts' focalPoint hasSegs = fronts
--     where

--         ssAll :: Vector ss
--         ssAll = fromList hasSegs

--         segAll :: Vector (Seg p)
--         segAll = V.map getSeg ssAll

--         ssAt :: SegIx -> ss
--         ssAt (SegIx ix) = ssAll ! ix

--         segAt :: SegIx -> Seg p
--         segAt (SegIx ix) = segAll ! ix

--         -- ## NOTE here the colinear points are filtered out ##
--         -- Get all the start/end point events
--         segToEvents :: SegIx -> [(EventType, Pos p, SegIx)]
--         segToEvents segIx = case (a - focalPoint) `crossZ` (b - a) of
--                                     0 -> []
--                                     z 
--                                         | z > 0     -> [(Start, a, segIx), (End,   b, segIx)]
--                                         | otherwise -> [(End,   a, segIx), (Start, b, segIx)]
--             where
--                 Seg a b = segAt segIx

--         unorderedEvents :: [(EventType, Pos p, SegIx)]
--         unorderedEvents = concatMap segToEvents [0..SegIx $ (V.length segAll) - 1]

--         -- Sort the events by Angle and store as [(start points and Segs, end points and segs)]
--         eventsSortedBySpread :: [(p, ([(Pos p, SegIx)], [(Pos p, SegIx)]))]
--         eventsSortedBySpread = sortOn fst [(the pointSpread, ([(p,i) | (Start,p,i) <- x], [(p,i) | (End,p,i) <- x]))
--                     | x@(_, pos, segIx) <- unorderedEvents
--                     , let pointSpread = unSpr $ spreadX2 (pos - focalPoint)
--                     , then group by pointSpread using groupWith]

--         -- Sort the events by Angle and store as [(start points and Segs, end points and segs)]
--         events :: [([(Pos p, SegIx)], [(Pos p, SegIx)])]
--         events = map snd $ eventsSortedBySpread

--         eventsSegIx :: [(S.Set SegIx, S.Set SegIx)]
--         eventsSegIx = map (\(new, old) -> (S.fromList (map snd new), S.fromList (map snd old))) events

--         -- Go through all the events once the get the hot edges just before the first event.
--         initialHotEdges :: S.Set SegIx
--         initialHotEdges = foldl
--                             (\hotEdges (new, old) -> (hotEdges `S.union` new) S.\\ old)
--                             S.empty
--                             eventsSegIx

--         -- Now that we have the initial hot edges, scan through the events again to get the hot edges for each wedge.
--         -- NOTE: the number of wedges == number of events
--         -- NOTE: Segs that end at an event are NOT included in the hot edges for the wedge.
--         wedgeHotEdges :: [S.Set SegIx]
--         wedgeHotEdges = tail $ scanl                -- tail is safe to do as the first element of scanl is initialHotEdges
--                             (\hotEdges (new, old) -> (hotEdges `S.union` new) S.\\ old)
--                             initialHotEdges
--                             eventsSegIx

--         -- There may be intersections in the wedges, so break down each wedge at the intersection poitns.
--         -- Since there are no start/end points within the wedge, intersection can be detected by the ordering
--         -- of segments' distances from the focal points at the start vs. end of the weged:
--         --
--         --        Wedge End
--         --                       ____ A ____________                                        
--         --             _B____C__/                                                           
--         --          __/                                                                    
--         --        /                                                                         
--         --     F =                                               ==> start: [A, B, C]                       
--         --         \ __                                              end:   [B, C, A]                       
--         --              |_______A_                                                          
--         --                         \______B___                   ==> C intersects A as their relative positions swap
--         --        Wedge Start                 \_C_____               B intersects A
--         --                                                           A intersects B and C

--         -- ### NOTE ###
--         -- For now we only care about the closest (to the focal point) segments (the shadow front) so we only need
--         -- to traverse the closest segments. In the example above:
--         -- 1. A is closest to F at the start of the wedge
--         -- 2. A intersects B and C
--         -- 3. Check which intersection happens first (in this case A intersects B before C).
--         -- 4. Break the wedge at the intersection of A and B
--         -- 5. The subwedge between the Start and the new break is correct
--         -- 6. Recurse to the subwedge between the new break and the end

--         wedgeRays :: [Ray p]  -- Representative point on each wedge ray
--         wedgeRays =
--             map
--                 (ray' focalPoint . (\repPoint -> repPoint - focalPoint)) -- we use ray' because we already filter out points that are on the focal points
--                 [head $ (map fst starts) ++ (map fst ends) | (starts, ends) <- events] -- head is possible since all events must be non-empty

--         wedgeStartEndRayCasts :: [([(Pos p, SegIx)], [(Pos p, SegIx)])]
--         wedgeStartEndRayCasts =
--             zipWith3
--                 (\rayStart rayEnd hotEdges ->
--                     let segs = S.toList hotEdges
--                     in (doRayCastWithLines rayStart segs, doRayCastWithLines rayEnd segs))
--                 wedgeRays
--                 (Utils.rotate 1 wedgeRays)
--                 wedgeHotEdges
--                 where
--                     -- TODO improve ray casting code to accept lines
--                     -- NOTE, we convert to lines in order to account for rounding errors
--                     doRayCastWithLines :: Ray p -> [SegIx] -> [(Pos p, SegIx)]
--                     doRayCastWithLines ray@(Ray f _) segIxs = 
--                         [(p, segIx)     | (line, segIx) <- zip lines segIxs
--                                         -- , let LRPoint p = traceShow ("initialHotEdges", initialHotEdges, ray, line, lineRayIntersection line ray) $ lineRayIntersection line ray  -- as the segs must not be colinear with the focalpoint, all intersections must be points
--                                         -- TODO: I managed to get this to fail once (using Floats): the result was not an LRPoint
--                                         , let LRPoint p = lineRayIntersection line ray  -- as the segs must not be colinear with the focalpoint, all intersections must be points
--                                         , then sortWith by quadrance (p - f)]
--                         where
--                             lines :: [Line p]
--                             lines = map (toLine . segAt) segIxs


--         dividedWedges :: [[(Seg p, SegIx)]]
--         dividedWedges = map (\case
--             ([], _)             -> []
--             ((front:_), end)    -> divideWedge front end) wedgeStartEndRayCasts
--             where
--                 divideWedge :: (Pos p, SegIx)       -- ^ current front point
--                             -> [(Pos p, SegIx)]     -- ^ end raycast
--                             -> [(Seg p, SegIx)]     -- ^ Front segments
--                 divideWedge front endRayCast = catMaybes $ divideWedge' front
--                     where
--                         endRayCastSegIxs = map snd endRayCast

--                         divideWedge' :: (Pos p, SegIx) -> [Maybe (Seg p, SegIx)]
--                         divideWedge' (frontP, frontSegIx) = currentFrontSegMay : maybe [] divideWedge' nextPSegIx
--                             where
--                                 intersectingSegIxs = takeWhile (/= frontSegIx) endRayCastSegIxs
--                                 intersectionExists = isJust nextPSegIx

--                                 -- current edge (may be nothing if the next point is equal to the current point
--                                 --      this can happen e.g. when 2 segments start at the same point on the start ray)
--                                 currentFrontSegMay :: Maybe (Seg p, SegIx)
--                                 currentFrontSegMay = do
--                                     nonPointSegment <- seg frontP nextP
--                                     return (nonPointSegment, frontSegIx)

--                                 nextP = maybe
--                                             (fst . fromJust . find ((==frontSegIx) . snd) $ endRayCast) -- current seg intersect with end ray
--                                             fst nextPSegIx


--                                 nextPSegIx = headMay [(p, segIx) | segIx <- intersectingSegIxs
--                                                         , let i = lineIntersection (toLine $ segAt segIx) (toLine $ segAt frontSegIx)
--                                                         , LPoint p <- [i]
--                                                         , then sortWith by (qd frontP p)]

--         -- Convert back to HasSegments
--         fronts :: [[ss]]
--         fronts
--             = map (map toSS . clean)
--             . filter (not . null)
--             . wrap
--             . map concat
--             . groupBy (\a b -> null a && null b)
--             . map concat
--             . splitOn [[]]
--             $ dividedWedges
--             where
--                 toSS :: (Seg p, SegIx) -> ss
--                 toSS (s, segIx) = setSeg s (ssAt segIx)

--                 clean :: [(Seg p, SegIx)] -> [(Seg p, SegIx)]
--                 clean fs
--                     = mapMaybe (\g -> let
--                                     Seg segStartP _ = fst $ head g
--                                     Seg _ segEndP   = fst $ last g
--                                  in do s <- seg segStartP segEndP; return (s, snd $ head g))
--                     . groupBy (\(_, aIx) (_, bIx) -> aIx == bIx)
--                     $ fs

--                 wrap :: Show a =>  [[a]] -> [[a]]
--                 wrap [] = []
--                 wrap [a] = [a]
--                 wrap orig@(x:xs) = ((last orig) ++ x) : init xs


newtype SegIx = SegIx { unSegIx :: Int } deriving (Eq, Show, Enum, Ord, Num)
data EventType = Start | End
    deriving (Eq, Show, Ord)
