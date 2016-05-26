{-# LANGUAGE ScopedTypeVariables #-}

import Data.List

import Geometry.Geometry
import Geometry.RayTrace
import Linear

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup  "Tests" [
          testsHUnit
        , testsQuickCheck
    ]

type TestType = Rational

r2 :: TestType -> TestType -> V2 TestType
r2 = V2

testsHUnit :: TestTree
testsHUnit = testGroup "Unit Tests" [
        
        testGroup "Primitives" [

              testCase "Line ==" $
                (Line (r2 100 200) (r2 1 2)) @?= (Line (r2 1 2) (r2 (-1) (-2)))

            , testCase "Line /=" $
                (Line (r2 100 200) (r2 1 2)) /= (Line (r2 1 2) (r2 1 (-2))) @?= True

            , testCase "Ray ==" $
                (Ray (r2 100 200) (r2 1 2)) @?= (Ray (r2 100 200) (r2 2 4))

            , testCase "Ray /=" $
                (Ray (r2 100 200) (r2 1 2)) /= (Ray (r2 100 200) (r2 (-1) (-2))) @?= True

            , testCase "Seg ==" $
                (Seg (r2 1 2) (r2 3 4)) @?= (Seg (r2 3 4) (r2 1 2))

            , testCase "Seg /=" $
                (Seg (r2 100 200) (r2 1 2)) /= (Seg (r2 100 200) (r2 (-1) (-2))) @?= True

        ]

        , testGroup "Line Line Intersect" [

              testCase "Line" $ let line = Line (r2 0 0) (r2 1 1) in
                lineIntersection
                    line
                    (Line (r2 2 2) (r2 (-1) (-1)))
                        @?= LLine line

            , testCase "Point" $
                lineIntersection
                    (Line (r2 0 0) (r2   1  1))
                    (Line (r2 2 0) (r2 (-1) 1))
                        @?= LPoint (r2 1 1)

            , testCase "Nothing" $
                lineIntersection
                    (Line (r2 0 0) (r2 2 0))
                    (Line (r2 1 1) (r2 2 0))
                        @?= LNothing
        ]
        
        , testGroup "Line Ray Intersect" [

              testCase "Ray" $ let ray = Ray (r2 4 2) (r2 (-2) (-1)) in
                lineRayIntersection
                    (Line (r2 0 0) (r2 2 1))
                    ray
                        @?= LRRay ray

            , testCase "Point (Ray Start)" $
                lineRayIntersection
                    (Line (r2 0 0) (r2 2 1))
                    (Ray  (r2 4 2) (r2 1 0))
                        @?= LRPoint (r2 4 2)

            , testCase "Point (further down the ray)" $
                lineRayIntersection
                    (Line (r2 0   1)  (r2 1 (-1)))
                    (Ray  (r2 0 (-1)) (r2 1   1))
                        @?= LRPoint (r2 1 0)
        ]
        
        , testGroup "Seg Seg Intersect" [

              testCase "End Points Touching" $
                segIntersection
                    (Seg (r2 0    0) (r2 13245 6745))
                    (Seg (r2 1345 0) (r2 13245 6745))
                        @?= SPoint (r2 13245 6745)
        ]

        , testGroup "Seg Ray Intersect" [

              testCase "Seg" $ let
                    seg = (Seg (r2 0 0) (r2 2 1))
                    ray = Ray (r2 4 2) (r2 (-2) (-1))
                in
                    segRayIntersection
                        seg
                        ray
                            @?= SRSeg seg

            , testCase "Point (Ray Start)" $
                segRayIntersection
                    (Seg (r2 0 0) (r2 4 2))
                    (Ray  (r2 4 2) (r2 1 0))
                        @?= SRPoint (r2 4 2)

            , testCase "Point (further down the ray)" $
                segRayIntersection
                    (Seg (r2 0   1)  (r2 2 (-1)))
                    (Ray  (r2 0 (-1)) (r2 1   1))
                        @?= SRPoint (r2 1 0)
        ]

        , testGroup "Ray Casting" [
            
            testCase "Many Segments 01" $
                let
                    ray = Ray (r2 1 1) (r2 2 1)
                    hitsAndSegsByDist = 
                        [(r2 1 1, Seg (r2 2 0) (r2 0 2))
                        ,(r2 3 2, Seg (r2 3 2) (r2 4 2))
                        ,(r2 5 3, Seg (r2 5 0) (r2 5 7))
                        ,(r2 7 4, Seg (r2 9 5) (r2 7 4))]   -- segment on line
                    misses = 
                        [Seg (r2 1 0) (r2 0 0.9)
                        ,Seg (r2 4 2) (r2 5 2)
                        ,Seg (r2 5 4) (r2 5 7)
                        ,Seg (r2 5 2) (r2 5 0)
                        ,Seg (r2 (-1) 0) (r2 (-3) (-1))]
                in rayTrace (misses ++ (reverse $ map snd hitsAndSegsByDist)) ray @?= hitsAndSegsByDist
        ]
    ]

testsQuickCheck :: TestTree
testsQuickCheck = testGroup "Property Tests" [

        testGroup "Primitives" [

            testGroup "Line" [

                  testProperty "a == a" $
                    (\(a :: Line TestType) -> a == a)
                    
                , testProperty "Line p d == Line p (d ^* s)    (s /= 0)" $
                    (\p d (NonZero (s :: TestType)) -> Line p d == Line p (d ^* s))
                    
                , testProperty "Line p d == Line (p + (d ^* s)) d" $
                    (\p d (s :: TestType) -> Line p d == Line (p + (d ^* s)) d)
                    
                , testProperty "Line p d /= Line (p + dp) d    (dp /= 0 and dp is not colinear with d)" $
                    (\p d (NonZero (dp :: V2 TestType)) -> d `crossZ` dp == 0 || Line p d /= Line (p + dp) d)
                    
                , testProperty "Line p d /= Line p (d + dd)    (dd /= 0 and dd is not colinear with d)" $
                    (\p d (NonZero (dd :: V2 TestType)) -> d `crossZ` dd == 0 || Line p d /= Line p (d + dd))
            ]

        ]

        , testGroup "Line Line Intersect" [

            testProperty "lineIntersection a b == lineIntersection b a" $
                (\a (b :: Line TestType) -> lineIntersection a b == lineIntersection b a)
        ]

        , testGroup "Seg Seg Intersect" [

            testProperty "Commutative" $
                (\a (b :: Seg TestType) -> segIntersection a b == segIntersection b a)

            , testProperty "segIntersection (Seg a b) (Seg b c) /= SNothing" $
                (\a b (c :: V2 TestType) -> segIntersection (Seg a b) (Seg b c) /= SNothing)

            , testProperty "Colinear Point Intersection" $
                (\p d (Positive lo') (Positive (hi :: TestType))
                    -> let lo = negate lo' in all (== SPoint p) [
                            segIntersection (Seg p (d ^* lo)) (Seg p (d ^* hi)),
                            segIntersection (Seg (d ^* lo) p) (Seg p (d ^* hi))
                        ])

            , testProperty "Colinear Sub-Segment Intersection" $
                (\p dir a' b' c' (d' :: TestType)
                    -> let [a,b,c,d] = map (\dp -> p + (dir ^* dp)) $ sort [a',b',c',d'] in
                        b == c ||
                        segIntersection (Seg a c) (Seg b c) == SSeg (Seg b c))

        ]

        , testGroup "Seg Ray Intersect" [

              testProperty "Reverse Ray" $
                (\(seg :: Seg TestType) ray@(Ray p d) -> case segRayIntersection seg ray of
                    SRPoint p' ->
                        p' == p ||
                        segRayIntersection seg (Ray p (negate d)) == SRNothing
                    _ -> True)

            , testProperty "Seg Ray Overlap" $
                (\(p :: V2 TestType) d ->
                    let seg = Seg (p + d) (p + 2 *^ d)
                    in segRayIntersection seg (Ray p d) == SRSeg seg)
        ]

        , testGroup "Point Seg Intersect" [

              testProperty "On First Seg Point" $
                (\(a :: V2 TestType) b ->
                    let seg = Seg a b
                    in pointSegIntersection a seg == Just a)

            , testProperty "On Second Seg Point" $
                (\(a :: V2 TestType) b ->
                    let seg = Seg a b
                    in pointSegIntersection b seg == Just b)

            , testProperty "On Midpoint Seg Point" $
                (\(a :: V2 TestType) b ->
                    let
                        seg = Seg a b
                        p = (a + b) ^/ 2
                    in pointSegIntersection p seg == Just p)

            , testProperty "Degenerate Seg" $
                (\(a :: V2 TestType) ->
                    let seg = Seg a a
                    in pointSegIntersection a seg == Just a)

            , testProperty "Off Edge" $
                (\(a :: V2 TestType) b offsetVector ->
                    -- ensure the offsetVector is not along the segment or at least is far enough to escape the segment.
                    offsetVector `crossZ` (a-b) > 0 || (quadrance offsetVector > quadrance (1-b)) ==>
                    pointSegIntersection (a + offsetVector) (Seg a b) == Nothing)
        ]
    ]

