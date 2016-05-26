{-# LANGUAGE ScopedTypeVariables #-}

module Geometry.RayTrace (

        rayTrace

    ) where

import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.List (minimumBy, sortOn)
import Geometry.Geometry
import Types
import Linear

rayTrace :: forall p. (Fractional p, Ord p)
    => [Seg p]
    -> Ray p
    -> [(Pos p, Seg p)]
rayTrace edges ray@(Ray rayStart _)
    -- Sort by distance to ray start
    = sortOn (qd rayStart . fst)
    -- Get all segment based ray traces.
    . mapMaybe rayTraceSeg
    $ edges
    where
        rayTraceSeg :: Seg p -> Maybe (Pos p, Seg p)
        rayTraceSeg seg = do
            intersectPoint <- toClosestPoint rayStart $ segRayIntersection seg ray
            return (intersectPoint, seg)

-- | Convert a segemnt-ray intersection to the colsest point if one exists.
toClosestPoint :: (Num p, Ord p)
    => Pos p                -- ^ ray trace start
    -> SegRayIntersect p    -- ^ intersection
    -> Maybe (Pos p)        -- ^ Closest point
toClosestPoint startPos intersection = case intersection of
    SRNothing           -> Nothing
    SRPoint p           -> Just p
    SRSeg seg@(Seg a b) -> case pointSegIntersection startPos seg of
                            Just p     -> Just p
                            otherwise  -> Just $ minimumBy (compare `on` (qd startPos)) [a, b]