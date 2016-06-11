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

        , chop2
        , ShadowFront(..)
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
import qualified Data.Vector as V
import Linear
import qualified Data.Set as S

import Debug.Trace


type ShadowFrontSeg l p = (l, Seg p) -- (l, spreadX2, squareDist from focal point, point)
-- list or segments ordered by SpreadX2 and split by empty space. Ther is no "visual" overlap about the focal point
-- elements always have atleadt 2 elements (a single segment)
-- first element is the START of a segment after SpreadX2 == 0 (i.e. the point after )
newtype ShadowFront l p = ShadowFront { unShadowFront :: [ShadowFrontSeg l p] }

toLabeledSegs :: ShadowFront l p -> [(l, Seg p)]
toLabeledSegs (ShadowFront xs) = xs

empty :: ShadowFront l p
empty = ShadowFront []






-- merge :: forall l p. Pos p -> ShadowFront l p -> ShadowFront l p -> ShadowFront l p
-- merge focalPoint front = merged
--     where
--         (ms, initBreakMay) = initialMergeState front



--         -- TODO at the very end, append the initBreakMay if applicable
--         -- not that the broken end seg is already incuded in the merge state data



--  -- THIS ISNT GOING SO WELL... MAY BE IT IS BETTER TO BREAK THE PROBLEM DOWN A BIT....
--     -- KEEP SEG INDEXES SO THAT YOU CAN EASILY CHOP AND REMERGE SEGMENTS (ALSO GETS RID FO THE NEED TO BE EXPLICITE ABOUT AN INITIAL BREAK)
--     -- BREAK INTO WEDGES AND SOLVE INDEPENDANTLY


--         merge' :: MergeState l p -> [ShadowFront l p]

--         merge' (MergeState xs Nothing  []) =  xs
--         merge' (MergeState [] xMay xs) =  maybeToList xMay ++ xs
--         -- Far is Nothing
--         merge' (MergeState allNears@((nearLabel, Seg currentP nearEndP):_) Nothing allFars@((_,farStartP):_))
--             = if isOnSpr farStartPSpr currentP nearEndPSpr

--                 -- next point is the farStartP
--                 -- break the near seg
--                 then
--                     let

--                         midPoint = fromMaybe nearEndP (headMay [p | LPoint p <- [lineIntersection (line' focalPoint farStartP) (line' currentP nearEndP)]])
--                         firstBrokenSegMay = do
--                             s <- seg currentP midPoint
--                             return (nearLabel, s)
--                         secondBrokenSegMay = do
--                             s <- seg midPoint nearEndP
--                             return (nearLabel, s)

--                         neadToFlip = focalPoint `qd` farStartP < focalPoint `qd` midPoint

--                     in maybeToList firstBrokenSegMay ++ if neadToFlip
--                         then merge' 
--                             (MergeState
--                                 allFars
--                                 (maybeToList secondBrokenSegMay ++ tail allNears)
--                                 J)

--                 -- next point is the nearEndP
--                 else (head allNears) : merge' $ MergeState (tail allNears) (Just $ head allFars) (tail allFars)
--             where
--                 currentSpr   = spreadX2 currentP
--                 nearEndPSpr  = spreadX2 nearEndP
--                 farStartPSpr = spreadX2 farStartP



--         -- Far is Just

--                     nextP = 
                    -- What is the next point (and hence next ray)

                    -- what are the intersection points of the current seg at the next ray

                    -- Do the current segs intersect b4 the next ray?

-- data MergeState l p
--     = MergeState
--         [ShadowFrontSeg l p]                                     -- ^ current near seg and future segs
--         (Maybe (ShadowFrontSeg l p)) [ShadowFrontSeg l p]    -- ^ current far seg and future segs
--

chop2 :: (Fractional p, Ord p) => Pos p -> Pos p -> Seg p -> (Maybe (Seg p), Maybe (Seg p))
chop2 focalPoint p s@(Seg a b)
    -- In Wedge
    | pS >= bS  = (Just s, Nothing)
    -- After Wedge
    | pS <= aS  = (Nothing, Just s)
    -- On Wedge
    | otherwise = maybe (Nothing, Nothing) (\mid -> (seg a mid, seg mid b)) midMay
    where
        pS = spreadX2 (p - focalPoint)
        aS = spreadX2 (a - focalPoint)
        bS = spreadX2 (b - focalPoint)

        midMay = do
            rayLine <- line focalPoint (p - focalPoint)
            mid
                <- case lineIntersection (toLine s) rayLine of
                    LPoint m -> Just m
                    _ -> Nothing
            let midS = spreadX2 (mid - focalPoint)
            if midS < aS || midS > bS
                then Nothing
                else Just mid

-- Labels IDs and Spreads
data LIDS l p = LIDS
    { lidsL    :: l
    , lidsID   :: SegIx
    , lidsSprs :: (Spr p, Spr p)
    }

toShadowFront :: forall l p. (Fractional p, Ord p, Show p) => Pos p -> [(l, Seg p)] -> ShadowFront l p    -- this is O(n log(n)) by using divide and conquer (and merge)
toShadowFront focalPoint lss = removeIDSprs (toShadowFront' (zipWith (\ix (l, s) -> ((SegIx ix,l), s)) [0..] lss))
    where
        removeIDSprs (ShadowFront ss) =  ShadowFront $ map (\(l, s) -> (lidsL l, s)) ss

        toShadowFront' :: (Fractional p, Ord p, Show p) => [((SegIx, l), Seg p)] -> ShadowFront (LIDS l p) p    -- this is O(n log(n)) by using divide and conquer (and merge)
        toShadowFront' ss = shadowFronts
            where
                shadowFronts = case ss of
                    []         -> empty
                    [(ixl, s)] -> if colinear s then empty else ShadowFront . map (tag ixl) . crossingX . direct $ s
                    ss         -> merge focalPoint (toShadowFront' ss1) (toShadowFront' ss2)

                (ss1, ss2) = splitAt (length ss `div` 2) ss

                colinear (Seg a b) = (a - focalPoint) `crossZ` (b - focalPoint) == 0
                direct s@(Seg a b) = if (a - focalPoint) `crossZ` (b - a) > 0 then s else seg' b a
                crossingX s@(Seg a b) = if isOnSpr (Spr 0) (spreadX2 $ a - focalPoint) (spreadX2 $ b - focalPoint)
                    then case lineIntersection <$> line a (b-a) <*> line focalPoint (V2 1 0) of
                        Just (LPoint mid) -> catMaybes [seg a mid, seg mid b]
                        _                 -> [s]
                    else [s]

                tag (ix,l) s@(Seg a b) = (LIDS l ix (spreadX2 $ a - focalPoint, spreadX2 $ b - focalPoint), s)

merge :: forall p l. (Ord p, Fractional p, Num p, Show p)
      => Pos p
      -> ShadowFront (LIDS l p) p
      -> ShadowFront (LIDS l p) p
      -> ShadowFront (LIDS l p) p
merge focalPoint (ShadowFront as) (ShadowFront bs) = ShadowFront . joinSegs $ merge' as bs
    where

        joinSegs :: [((LIDS l p), Seg p)] -> [((LIDS l p), Seg p)]
        joinSegs = mapMaybe joinSegsGroup
                 . groupBy ((==) `on` lidsID . fst)

        joinSegsGroup :: [((LIDS l p), Seg p)] -> Maybe ((LIDS l p), Seg p)
        joinSegsGroup [x]                                = Just x
        joinSegsGroup ((LIDS l i (spra, _), Seg a _):xs) = sequence (LIDS l i (spra, sprb), seg a b)
            where
                (LIDS _ _ (_, sprb), Seg _ b) = last xs

        merge' [] bs = bs
        merge' as [] = as
        merge' (als@(al, a@(Seg a1 a2)):as) (bls@(bl, b@(Seg b1 b2)):bs)
            -- | a2S <= b1S      = als : merge' as (bls:bs)
            -- | b2S <= a1S      = bls : merge' (als:as) bs
            -- | a1S == b1S      = case segIntersection a b of
            --         SSeg s -> case compare a2S b2S of
            --             EQ -> als : merge' as bs
            --             LT -> (al, s) : merge' as ((maybeToList $ sequence (bl, seg a2 b2))++bs)
            --             GT -> (bl, s) : merge' ((maybeToList $ sequence (al, seg b2 a2))++as) bs

            --         SPoint p -> case (compare a1Q b1Q, compare a2S b2S, compare a2Q b2Q) of
            --             (LT, _ , _ ) ->
            --             (GT, _ , _ ) ->
            --             (EQ, EQ, EQ) ->
            --             (EQ, EQ, EQ) ->
            --             (EQ, EQ, EQ) ->
            --             (EQ, EQ, EQ) ->

            --         SNothing -> case (compare a1Q b1Q, compare a2S b2S) of
            --             (EQ, EQ) -> als : merge' as bs
            --             (EQ, LT) -> 

            -- | a1S < b1S       = s  : merge' (a':as) (b':bs)


            = case (aChop1May, aChop2May, bChop1May, bChop2May) of
                (Just aIn,  aOutMay,   Nothing,        _)                                   -> aIn : merge' (maybeToList aOutMay ++ as) (bls:bs)
                ( Nothing,        _,  Just bIn,  bOutMay)                                   -> bIn : merge' (als:as) (maybeToList bOutMay ++ bs)
                (Just aIn@(aInl, Seg aInA aInB),  aOutMay,  Just bIn@(bInl, Seg bInA bInB),  bOutMay)   -> merged ++ merge' (maybeToList bOutMay ++ bs) (maybeToList aOutMay ++ as)
                    where

                        merged
                            | aInAQ <= bInAQ && aInBQ <= bInBQ  = [aIn]
                            | aInAQ  > bInAQ && aInBQ  > bInBQ  = [bIn]
                            | otherwise                         = catMaybes [firstSegMay, secondSegMay]
                            
                        aInAQ = aInA `qd` focalPoint
                        aInBQ = aInB `qd` focalPoint
                        bInAQ = bInA `qd` focalPoint
                        bInBQ = bInB `qd` focalPoint

                        midSpr = spreadX2 $ mid - focalPoint
                        mid = case lineIntersection (toLine $ snd aIn) (toLine $ snd bIn) of
                                LPoint p -> p
                                _        -> error "always expected a point intersection in a wedge."


                        firstSegMay = if aInAQ < bInAQ
                            then (aInl { lidsSprs = (fst $ lidsSprs aInl, midSpr)},) <$> seg aInA mid
                            else (bInl { lidsSprs = (fst $ lidsSprs bInl, midSpr)},) <$> seg bInA mid

                        secondSegMay = if aInBQ < bInBQ
                            then (aInl { lidsSprs = (midSpr, snd $ lidsSprs aInl)},) <$> seg mid aInB
                            else (bInl { lidsSprs = (midSpr, snd $ lidsSprs bInl)},) <$> seg mid bInB

                -- Rounding error cases
                (Nothing,   Nothing,         _,        _)  -> merge' as (bls:bs)
                (      _,         _,   Nothing,  Nothing)  -> merge' (als:as) bs
                _ -> error "Not matching chop results"

            where
                a1Q = a1 `qd` focalPoint
                a2Q = a2 `qd` focalPoint
                b1Q = b1 `qd` focalPoint
                b2Q = b2 `qd` focalPoint

                -- assume that not all spreads are equal
                -- get the first and second spread and representative points
                (_:((chopS, chopP):_):_)
                    = groupBy (\(c1S,_) (c2S, _) -> c1S == c2S)
                    . sortOn fst
                    $ [(fst $ lidsSprs al, a1),
                       (snd $ lidsSprs al, a2),
                       (fst $ lidsSprs bl, b1),
                       (snd $ lidsSprs bl, b2)]

                -- chop
                (aChop1May, aChop2May) = chop chopP als
                (bChop1May, bChop2May) = chop chopP bls

        -- TODO: chop both... snds go into recursive call, fsts are merged: 1 -> just 1, 2 -> intersection?

        chop :: Pos p -> (LIDS l p, Seg p) -> (Maybe (LIDS l p, Seg p), Maybe (LIDS l p, Seg p))
        chop p lidsSeg@(LIDS l ix (aS, bS), s@(Seg a b))
            -- In Wedge
            | pS >= bS  = (Just lidsSeg, Nothing)
            -- After Wedge
            | pS <= aS  = (Nothing, Just lidsSeg)
            -- On Wedge
            | otherwise = maybe (Nothing, Nothing) (\(mid, midS) -> (
                                                        sequence (LIDS l ix (aS, midS), seg a mid),
                                                        sequence (LIDS l ix (midS, bS), seg mid b))) midmidSMay
            where
                pS = spreadX2 (p - focalPoint)

                midmidSMay = do
                    rayLine <- line focalPoint (p - focalPoint)
                    mid
                        <- case lineIntersection (toLine s) rayLine of
                            LPoint m -> Just m
                            _ -> Nothing
                    let midS = spreadX2 (mid - focalPoint)
                    if midS < aS || midS > bS
                        then Nothing
                        else Just (mid, midS)


        -- chop p s@(Seg a b) = let sp = spreadX2 (p - focalPoint)
        --     in case (compare sp $ spreadX2 (a - focalPoint), compare sp $ spreadX2 (b - focalPoint))
        --         (LT, LT) -> (Nothing, Just s)
        --         (LT, EQ) -> (Nothing, Just s)
        --         (GT, GT) -> (Just s, Nothing)
        --         (EQ, GT) -> (Just s, Nothing)
        --         (LT, GT) -> case lineIntersection <$> line focalPoint (p - focalPoint) <*> line a (b - a) of
        --             LPoint mid -> (seg a mid, seg mid b)
        --             _          -> (Nothing, Nothing)
        --         _        -> (Nothing, Nothing)

filterByFront :: ShadowFront l p -> ShadowFront l p -> ShadowFront l p
filterByFront = undefined

-- toShadowFront :: (Fractional p, Ord p) => Pos p -> [(l, Seg p)] -> ShadowFront l p    -- this is O(n log(n)) by using divide and conquer (and merge)
-- toShadowFront focalPoint xs
--     = fromMaybe empty
--     . headMay
--     . reduceToShadowFront'
--     . map toSingletonFront
--     . filter notColinear
--     $ xs
--     where
--         notColinear (_, Seg a b) = (a - focalPoint) `crossZ` (b - focalPoint) /= 0
--         toSingletonFront (l, s@(Seg a b)) = ShadowFront [(l, if (a - focalPoint) `crossZ` (b - a) > 0 then s else seg' b a)]

--         reduceToShadowFront' :: [ShadowFront l p] -> [ShadowFront l p]
--         reduceToShadowFront' xs' = map merge' . chunksOf 2 $ xs'

--         merge' [a]   = a
--         merge' [a,b] = merge a b

-- Helpers
-- flipNature :: SegNature -> SegNature
-- flipNature Near = Far
-- flipNature Far  = Near
-- data SegNature = Near | Far
--     deriving (Show)

-- data StartSegBreak l p = StartSegBreak SegNature (ShadowFrontSeg l p)
--     deriving (Show)

-- data MergeState l p
--     = MergeState
--         [ShadowFrontSeg l p]                                    -- ^ current near seg and future segs
--         (Maybe (ShadowFrontSeg l p)) [ShadowFrontSeg l p]   -- ^ current far seg and future segs
--     deriving (Show)

-- initialMergeState :: forall l p. (Fractional p, Ord p)
--      => Pos p                                           -- ^ focal point
--      -> ShadowFront l p                             -- ^ first front
--      -> ShadowFront l p                             -- ^ second front
--      -> (MergeState l p, Maybe (StartSegBreak l p))       -- ^ initial merge state and possible broken segment
-- -- assume that the inputs are non-empty
-- initialMergeState focalPoint (ShadowFront allAs@((a@(al, Seg a1 a2)) : as)) (ShadowFront allBs@((b@(bl, Seg b1 b2)) : bs))
--     = if aSpread == bSpread
--         then (MergeState (a:as) (Just b) bs, Nothing)         -- is this needed?
--         else if traceShowId brokenFirstFrontStartsOnNonEmptySpace
--             then if brokenFirstFrontP `qd` focalPoint > secondFrontPoint `qd` focalPoint
--                 then (MergeState secondFront      (Just $ head brokenFirstFront) (tail brokenFirstFront)
--                         , StartSegBreak Far  <$> brokenFirstFrontSegMay)
--                 else (MergeState brokenFirstFront (Just $ head secondFront)      (tail secondFront)
--                         , StartSegBreak Near <$> brokenFirstFrontSegMay)
--             else (MergeState secondFront Nothing brokenFirstFront, StartSegBreak Far <$> brokenFirstFrontSegMay)
--     where

--         aSpread = spreadX2 a1
--         bSpread = spreadX2 b1

--         (firstFront, secondFront, secondFrontSpr, secondFrontPoint) = if aSpread > bSpread
--             then (allBs, allAs, aSpread, a1)
--             else (allAs, allBs, bSpread, b1)

--         -- rotate the first front till the second front's point is on or before the rotated second fronts first segment.
--         -- If no such rotaton exists, then the second spred must be at the end of all the first front's segments (in an empty space).
--         (brokenFirstFront@((_, Seg brokenFirstFrontP _):_), brokenFirstFrontSegMay, brokenFirstFrontStartsOnNonEmptySpace) = breakAt secondFrontSpr secondFrontPoint firstFront

--         breakAt :: Spr p -> Pos p -> [ShadowFrontSeg l p] -> ([ShadowFrontSeg l p], Maybe (ShadowFrontSeg l p), Bool)
--         breakAt spr sprP (xs) = fromMaybe (xs, Nothing, False) $ headMay $ mapMaybe isGoodStart (rotations xs)
--             where
--                 isGoodStart :: [ShadowFrontSeg l p] -> Maybe ([ShadowFrontSeg l p], Maybe (ShadowFrontSeg l p), Bool)
--                 isGoodStart frontSegs@((l, Seg c d):rest)
--                     | sprC > spr                = Just (frontSegs, Nothing, False)
--                     | isOnSpr spr sprC sprD   = case lineIntersection (line' focalPoint sprP) (line' c (d - c)) of
--                                                             LPoint p    -> let breakSegMay = do s <- seg c p; return (l, s)
--                                                                             in Just (((maybeToList $ do s <- seg p d; return (l, s)) ++ rest ++ (maybeToList breakSegMay)), breakSegMay, True)
--                                                             -- anything else just don't break... This case is mathematically impossible, but may happen due to rounding errors.
--                                                             _           -> Just (frontSegs, Nothing, True)
                                                    
--                     | otherwise                 = Nothing
--                     where
--                         sprC = spreadX2 c
--                         sprD = spreadX2 d






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

shadowFronts :: (Ord p, Num p, Fractional p, Show p, HasSeg ss p)
             => Pos p -> [ss] -> [ss]
shadowFronts focalPoint hasSegs
    = map (\(l, s) -> setSeg s l)
    . unShadowFront
    . toShadowFront focalPoint
    . map (\hs -> (hs, getSeg hs))
    $ hasSegs

shadowFronts' :: forall ss p. (Ord p, Num p, Fractional p, HasSeg ss p

                    , Show ss, Show p)

                => Pos p          -- ^ focal point
                -> [ss]           -- ^ all the segments
                -> [[ss]]         -- ^ front segments
shadowFronts' focalPoint hasSegs = fronts
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
