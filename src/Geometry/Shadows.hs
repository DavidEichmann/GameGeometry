{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Geometry.Shadows (
          shadowFronts
        , toShadowFront
        , merge

        , ShadowFront(..)
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


type ShadowFrontSeg l p = (l, Seg p) -- (l, spreadX2, squareDist from focal point, point)
-- list or segments ordered by SpreadX2 and split by empty space. Ther is no "visual" overlap about the focal point
-- elements always have atleadt 2 elements (a single segment)
-- first element is the START of a segment after SpreadX2 == 0 (i.e. the point after )
newtype ShadowFront l p = ShadowFront { unShadowFront :: [ShadowFrontSeg l p] }
    deriving (Show)

toLabeledSegs :: ShadowFront l p -> [(l, Seg p)]
toLabeledSegs (ShadowFront xs) = xs

empty :: ShadowFront l p
empty = ShadowFront []

toShadowFront :: forall l p. (Fractional p, Ord p, Show p, Show l) => Pos p -> [(l, Seg p)] -> ShadowFront l p    -- this is O(n log(n)) by using divide and conquer (and merge)
toShadowFront _ [] = empty
toShadowFront focalPoint [(l, s@(Seg a b))] = if colinear s 
    then empty
    else ShadowFront . map tag . crossingX . direct $ s
    where
        colinear (Seg a b) = (a - focalPoint) `crossZ` (b - focalPoint) == 0
        direct s@(Seg a b) = if (a - focalPoint) `crossZ` (b - a) > 0 then s else seg' b a
        crossingX s@(Seg a b) = if isOnSpr (Spr 0) (spreadX2 $ a - focalPoint) (spreadX2 $ b - focalPoint)
            then case lineIntersection (toLine s) (line' focalPoint (V2 1 0)) of
                LPoint mid -> catMaybes [seg a mid, seg mid b]
                _          -> []
            else [s]
        tag s = (l, s)
toShadowFront focalPoint lss = merge focalPoint (toShadowFront focalPoint lss1) (toShadowFront focalPoint lss2)
    where
        (lss1, lss2) = splitAt (length lss `div` 2) lss

-- toShadowFront focalPoint lss = relabel (toShadowFront' (zipWith (\ix (l, s) -> ((SegIx ix,l), s)) [0..] lss))
--     where
--         labels = V.fromList (map fst lss)
--         relabel (ShadowFront ss) =  ShadowFront $ map (\(ix, s) -> (labels!ix, s)) ss

--         toShadowFront' :: (Fractional p, Ord p, Show p) => [(SegIx, Seg p)] -> ShadowFront SegIx p    -- this is O(n log(n)) by using divide and conquer (and merge)
--         toShadowFront' ss = shadowFronts
--             where
--                 shadowFronts = case ss of
--                     []         -> empty
--                     [(ix, s)] -> if colinear s then empty else ShadowFront . map (tag ix) . crossingX . direct $ s
--                     ss         -> merge focalPoint (toShadowFront' ss1) (toShadowFront' ss2)

--                 (ss1, ss2) = splitAt (length ss `div` 2) ss

--                 colinear (Seg a b) = (a - focalPoint) `crossZ` (b - focalPoint) == 0
--                 direct s@(Seg a b) = if (a - focalPoint) `crossZ` (b - a) > 0 then s else seg' b a
--                 crossingX s@(Seg a b) = if isOnSpr (Spr 0) (spreadX2 $ a - focalPoint) (spreadX2 $ b - focalPoint)
--                     then case lineIntersection (toLine s) <$> line focalPoint (V2 1 0) of
--                         Just (LPoint mid) -> catMaybes [seg mid b, seg a mid]
--                         _                 -> []
--                     else [s]

--                 tag ix s@(Seg a b) = (LIDS l ix ((spreadX2 $ a - focalPoint, a), (let Spr sB = spreadX2 $ b - focalPoint in if sB == 0 then Spr 4 else Spr sB, b)), s)

-- TODO look for incorrect Spreads in merge!!!!!!!!!!!!!!

-- a segment represented by a spread(P) range and a line
data WLine l p = WLine l ((Spr p,Pos p), (Spr p, Pos p)) (Line p)
    deriving (Show)

toWLine :: (Ord p, Fractional p) => Pos p -> (l, Seg p) -> WLine l p
toWLine focalPoint (l, s@(Seg a b)) = WLine l ((spreadX2 $ a - focalPoint, a), (spreadX2 $ b - focalPoint, b)) (toLine s)

merge :: forall p l. (Ord p, Fractional p, Num p, Show p, Show l)
      => Pos p
      -> ShadowFront l p
      -> ShadowFront l p
      -> ShadowFront l p
merge focalPoint (ShadowFront as) (ShadowFront bs) = ShadowFront . joinSegs $ merge'' (map (toWLine focalPoint) as) (map (toWLine focalPoint) bs)
    where





        -- TODO merge spreads across the positive x ray
        joinSegs = id





        -- joinSegs' :: [((LIDS l p), Seg p)] -> [((LIDS l p), Seg p)]
        -- joinSegs' = mapMaybe joinSegsGroup
        --          . groupBy ((==) `on` lidsID . fst)

        -- joinSegsGroup :: [((LIDS l p), Seg p)] -> Maybe ((LIDS l p), Seg p)
        -- joinSegsGroup [x]                                = Just x
        -- joinSegsGroup ((LIDS l i (spra, _), Seg a _):xs) = sequence (LIDS l i (spra, sprb), seg a b)
        --     where
        --         (LIDS _ _ (_, sprb), Seg _ b) = last xs





        merge'' :: [WLine l p] -> [WLine l p] -> [(l, Seg p)]
        merge'' as bs = concatMap mergeWedge $ toWedges as bs

        -- break into wedges
        toWedges :: [WLine l p]
                 -> [WLine l p]
                 -> [(((Spr p, Pos p), (Spr p, Pos p)), Either (l, Line p) ((l, Line p), (l, Line p)))]
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

        mergeWedge :: (((Spr p, Pos p), (Spr p, Pos p)), Either (l, Line p) ((l, Line p), (l, Line p)))
                   -> [(l, Seg p)]
        mergeWedge (((aS, aP), (bS, bP)), wlines)
            = case wlines of
                -- Single segment, so try and find points of intersection with wedge rays
                Left (cL, cLine) -> maybeToList' "a" $ do
                    aRay <- ray focalPoint (aP - focalPoint)
                    bRay <- ray focalPoint (bP - focalPoint)
                    pointA <- case lineRayIntersection cLine aRay of LRPoint p -> Just p; _ -> Nothing
                    pointB <- case lineRayIntersection cLine bRay of LRPoint p -> Just p; _ -> Nothing
                    s <- seg pointA pointB
                    return (cL, s)

                Right ((uL, uLine), (vL, vLine)) -> fromMaybe [] $ do
                    -- get wedge rays
                    aRay <- ray focalPoint (aP - focalPoint)
                    bRay <- ray focalPoint (bP - focalPoint)

                    -- Ray cast along the 2 wedge rays against the 2 segments
                    return $ case (lineRayIntersection uLine aRay, lineRayIntersection uLine bRay, lineRayIntersection vLine aRay, lineRayIntersection vLine bRay) of
                        
                        -- The case we expect!
                        (LRPoint uaP, LRPoint ubP, LRPoint vaP, LRPoint vbP) ->
                            let
                                full1May = (uL,) <$> seg uaP ubP
                                full2May = (vL,) <$> seg vaP vbP
                                midPMay  = case lineIntersection uLine vLine of LPoint p -> Just p; _ -> Nothing
                                arbMay   = full1May `mplus` full2May
                            in case (compare (uaP `qd` focalPoint) (vaP `qd` focalPoint), compare (ubP `qd` focalPoint) (vbP `qd` focalPoint)) of
                                -- Segments are on top of each other... just pick an arbitrary one
                                (EQ, EQ) -> maybeToList' "b" arbMay

                                -- = shape. take the closer one
                                (LT, LT) -> maybeToList' "c" full1May
                                (GT, GT) -> maybeToList' "d" full2May

                                -- > or < shape. Pick the one that is in front on at least one side
                                (EQ, LT) -> maybeToList' "e" $ full1May `mplus` full2May
                                (EQ, GT) -> maybeToList' "f" $ full2May `mplus` full1May
                                (LT, EQ) -> maybeToList' "g" $ full1May `mplus` full2May
                                (GT, EQ) -> maybeToList' "h" $ full2May `mplus` full1May

                                -- X shape.
                                -- if mid point is invalid, then just use arbitrary full segment
                                (LT, GT) -> maybe (maybeToList' "i" arbMay) (\midP -> catMaybes [
                                        (uL,) <$> seg uaP midP,
                                        (vL,) <$> seg midP vbP
                                    ]) midPMay
                                (GT, LT) -> maybe (maybeToList' "j" arbMay) (\midP -> catMaybes [
                                        (vL,) <$> seg vaP midP,
                                        (uL,) <$> seg midP ubP
                                    ]) midPMay

                        -- one of the segs appears invalid... just use the other seg
                        (LRPoint uaP, LRPoint ubP, _, _) -> maybeToList' "k" $ (uL,) <$> seg uaP ubP
                        (_, _, LRPoint vaP, LRPoint vbP) -> maybeToList' "l" $ (vL,) <$> seg vaP vbP
                        -- both segs appear invalid... drop this wedge
                        _ -> []





        -- merge' [] bs = bs
        -- merge' as [] = as
        -- merge' (als@(al, a@(Seg a1 a2)):as) (bls@(bl, b@(Seg b1 b2)):bs)
        --     = case (aChop1May, aChop2May, bChop1May, bChop2May) of
        --         (Just aIn,  aOutMay,   Nothing,        _)                                   -> aIn : merge' (maybeToList aOutMay ++ as) (bls:bs)
        --         ( Nothing,        _,  Just bIn,  bOutMay)                                   -> bIn : merge' (als:as) (maybeToList bOutMay ++ bs)
        --         (Just aIn@(aInl, Seg aInA aInB),  aOutMay,  Just bIn@(bInl, Seg bInA bInB),  bOutMay)   -> merged ++ merge' (maybeToList bOutMay ++ bs) (maybeToList aOutMay ++ as)
        --             where

        --                 merged
        --                     | aInAQ <= bInAQ && aInBQ <= bInBQ  = [aIn]
        --                     | aInAQ  > bInAQ && aInBQ  > bInBQ  = [bIn]
        --                     | otherwise                         = catMaybes [firstSegMay, secondSegMay]
                            
        --                 aInAQ = aInA `qd` focalPoint
        --                 aInBQ = aInB `qd` focalPoint
        --                 bInAQ = bInA `qd` focalPoint
        --                 bInBQ = bInB `qd` focalPoint

        --                 midSpr = spreadX2 $ mid - focalPoint
        --                 mid = case lineIntersection (toLine $ snd aIn) (toLine $ snd bIn) of
        --                         LPoint p -> p
        --                         _        -> error "always expected a point intersection in a wedge."


        --                 firstSegMay = if aInAQ < bInAQ
        --                     then (aInl { lidsSprs = (fst $ lidsSprs aInl, midSpr)},) <$> seg aInA mid
        --                     else (bInl { lidsSprs = (fst $ lidsSprs bInl, midSpr)},) <$> seg bInA mid

        --                 secondSegMay = if aInBQ < bInBQ
        --                     then (aInl { lidsSprs = (midSpr, snd $ lidsSprs aInl)},) <$> seg mid aInB
        --                     else (bInl { lidsSprs = (midSpr, snd $ lidsSprs bInl)},) <$> seg mid bInB

        --         -- Rounding error cases
        --         (Nothing,   Nothing,         _,        _)  -> merge' as (bls:bs)
        --         (      _,         _,   Nothing,  Nothing)  -> merge' (als:as) bs
        --         _ -> error "Not matching chop results"

        --     where
        --         a1Q = a1 `qd` focalPoint
        --         a2Q = a2 `qd` focalPoint
        --         b1Q = b1 `qd` focalPoint
        --         b2Q = b2 `qd` focalPoint

        --         -- assume that not all spreads are equal
        --         -- get the first and second spread and representative points
        --         (_:((chopS, chopP):_):chopSPs)
        --             = groupBy (\(c1S,_) (c2S, _) -> c1S == c2S)
        --             . sortOn fst
        --             $ [(fst $ lidsSprs al, a1),
        --                (snd $ lidsSprs al, a2),
        --                (fst $ lidsSprs bl, b1),
        --                (snd $ lidsSprs bl, b2)]

        --         (chopS', chopP') = head $ head chopSPs

        --         -- chop
        --         ((aChop1May, aChop2May), (bChop1May, bChop2May))
        --             = let choped = (chop chopP als, chop chopP bls)
        --             in case choped of
        --                 ((Nothing, Just _), (Nothing, Just _)) -> (chop chopP' als, chop chopP' bls)
        --                 _                                      -> choped


        -- -- TODO: chop both... snds go into recursive call, fsts are merged: 1 -> just 1, 2 -> intersection?

        -- chop :: Pos p -> (LIDS l p, Seg p) -> (Maybe (LIDS l p, Seg p), Maybe (LIDS l p, Seg p))
        -- chop p lidsSeg@(LIDS l ix (aS, bS), s@(Seg a b))
        --     -- In Wedge
        --     | pS >= bS  = (Just lidsSeg, Nothing)
        --     -- After Wedge
        --     | pS <= aS  = (Nothing, Just lidsSeg)
        --     -- On Wedge
        --     | otherwise = maybe (Nothing, Nothing) (\(mid, midS) -> (
        --                                                 sequence (LIDS l ix (aS, midS), seg a mid),
        --                                                 sequence (LIDS l ix (midS, bS), seg mid b))) midmidSMay
        --     where
        --         pS = spreadX2 (p - focalPoint)

        --         midmidSMay = do
        --             rayLine <- line focalPoint (p - focalPoint)
        --             mid
        --                 <- case lineIntersection (toLine s) rayLine of
        --                     LPoint m -> Just m
        --                     _ -> Nothing
        --             let midS = spreadX2 (mid - focalPoint)
        --             if midS < aS || midS > bS
        --                 then Nothing
        --                 else Just (mid, midS)

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

shadowFronts :: (Ord p, Num p, Fractional p, Show p, Show ss, HasSeg ss p)
             => Pos p -> [ss] -> [ss]
shadowFronts focalPoint hasSegs
    = map (\(l, s) -> setSeg s l)
    . unShadowFront
    . toShadowFront focalPoint
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
