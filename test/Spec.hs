{-# LANGUAGE ScopedTypeVariables #-}

import           Data.List
import           Data.Fixed (mod')

import qualified Utils as Utils
import           Geometry.Angles
import           Geometry.Geometry
import           Geometry.RayTrace
import           Geometry.Shadows
import           Linear                hiding (rotate)

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

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
                (line' (r2 100 200) (r2 1 2)) @?= (line' (r2 1 2) (r2 (-1) (-2)))

            , testCase "Line /=" $
                (line' (r2 100 200) (r2 1 2)) /= (line' (r2 1 2) (r2 1 (-2))) @?= True

            , testCase "Ray ==" $
                (ray' (r2 100 200) (r2 1 2)) @?= (ray' (r2 100 200) (r2 2 4))

            , testCase "Ray /=" $
                (ray' (r2 100 200) (r2 1 2)) /= (ray' (r2 100 200) (r2 (-1) (-2))) @?= True

            , testCase "Seg ==" $
                (seg' (r2 1 2) (r2 3 4)) @?= (seg' (r2 3 4) (r2 1 2))

            , testCase "Seg /=" $
                (seg' (r2 100 200) (r2 1 2)) /= (seg' (r2 100 200) (r2 (-1) (-2))) @?= True

            , testCase "Polygon ==" $
                (polygon [r2 1 1, r2 2 2, r2 (-5) (-4)] @?= polygon [r2 1 1, r2 (-5) (-4), r2 2 2])

            , testCase "Polygon /=" $
                (polygon [r2 1 1, r2 2 2, r2 (-5) (-4)] /= polygon [r2 1 1, r2 (-5) (-4), r2 3 2]) @?= True

        ]

        , testGroup "Line Line Intersect" [

              testCase "Line" $ let l = line' (r2 0 0) (r2 1 1) in
                lineIntersection
                    l
                    (line' (r2 2 2) (r2 (-1) (-1)))
                        @?= LLine l

            , testCase "Point" $
                lineIntersection
                    (line' (r2 0 0) (r2   1  1))
                    (line' (r2 2 0) (r2 (-1) 1))
                        @?= LPoint (r2 1 1)

            , testCase "Nothing" $
                lineIntersection
                    (line' (r2 0 0) (r2 2 0))
                    (line' (r2 1 1) (r2 2 0))
                        @?= LNothing
        ]

        , testGroup "Line Ray Intersect" [

              testCase "Ray" $ let r = ray' (r2 4 2) (r2 (-2) (-1)) in
                lineRayIntersection
                    (line' (r2 0 0) (r2 2 1))
                    r
                        @?= LRRay r

            , testCase "Point (Ray Start)" $
                lineRayIntersection
                    (line' (r2 0 0) (r2 2 1))
                    (ray'  (r2 4 2) (r2 1 0))
                        @?= LRPoint (r2 4 2)

            , testCase "Point (further down the ray)" $
                lineRayIntersection
                    (line' (r2 0   1)  (r2 1 (-1)))
                    (ray'  (r2 0 (-1)) (r2 1   1))
                        @?= LRPoint (r2 1 0)
        ]

        , testGroup "Seg Seg Intersect" [

              testCase "End Points Touching" $
                segIntersection
                    (seg' (r2 0    0) (r2 13245 6745))
                    (seg' (r2 1345 0) (r2 13245 6745))
                        @?= SPoint (r2 13245 6745)
        ]

        , testGroup "Seg Ray Intersect" [

              testCase "Seg" $ let
                    s = (seg' (r2 0 0) (r2 2 1))
                    r = ray' (r2 4 2) (r2 (-2) (-1))
                in
                    segRayIntersection
                        s
                        r
                            @?= SRSeg s

            , testCase "Point (Ray Start)" $
                segRayIntersection
                    (seg' (r2 0 0) (r2 4 2))
                    (ray' (r2 4 2) (r2 1 0))
                        @?= SRPoint (r2 4 2)

            , testCase "Point (further down the ray)" $
                segRayIntersection
                    (seg' (r2 0   1)  (r2 2 (-1)))
                    (ray' (r2 0 (-1)) (r2 1   1))
                        @?= SRPoint (r2 1 0)
        ]

        , testGroup "Ray Casting" [

            testCase "Many Segments 01" $
                let
                    r = ray' (r2 1 1) (r2 2 1)
                    hitsAndSegsByDist =
                        [(r2 1 1, seg' (r2 2 0) (r2 0 2))
                        ,(r2 3 2, seg' (r2 3 2) (r2 4 2))
                        ,(r2 5 3, seg' (r2 5 0) (r2 5 7))
                        ,(r2 7 4, seg' (r2 9 5) (r2 7 4))]   -- segment on line
                    misses =
                        [seg' (r2 1 0) (r2 0 0.9)
                        ,seg' (r2 4 2) (r2 5 2)
                        ,seg' (r2 5 4) (r2 5 7)
                        ,seg' (r2 5 2) (r2 5 0)
                        ,seg' (r2 (-1) 0) (r2 (-3) (-1))]
                in rayTrace (misses ++ (reverse $ map snd hitsAndSegsByDist)) r @?= hitsAndSegsByDist
        ]

        , testGroup "Point inside Polygon" [

              testCase "Completely inside / outside" $
                let p = r2 1 1
                    a = polygon [r2 0 0, r2 2 0, r2 2 2, r2 0 2]
                    b = polygon [r2 3 3, r2 2 3, r2 2 2, r2 3 2]
                in pointInside p a && not (pointInside p b) @?= True

            , testCase "On an edge / vertex" $
                let p = r2 1 1
                    a = polygon [r2 0 0, r2 1 0, r2 1 1, r2 0 1]
                    b = polygon [r2 0 0, r2 1 0, r2 1 2, r2 0 2]
                in pointInside p a || pointInside p b @?= False

            , testCase "Vertex on the right (special case)" $
                pointInside (r2 1 1) (polygon [r2 1 0, r2 2 1, r2 1 2, r2 0 1]) @?= True
        ]

        , testGroup "Shadow fronts" [

              testCase "Single Seg" $
                let
                    focalPoint = r2 1 1
                    a = seg' (r2  2 1) (r2  1 2)
                    segs = [a]
                    expected = [a]
                in elem (shadowFronts focalPoint segs) (Utils.rotations expected) @?= True

            , testCase "2 Segs" $
                let
                    focalPoint = r2 1 1
                    segs@[a,b] = 
                        [ seg' (r2   2  1) (r2   1    2)
                        , seg' (r2 (-2) 0) (r2 (-2) (-2))
                        ]
                    expected = [a,b]
                in elem (shadowFronts focalPoint segs) (Utils.rotations expected) @?= True

            , testCase "3 disjoint Segs" $
                let
                    focalPoint = r2 1 1
                    segs@[a,b,c] = 
                        [ seg' (r2 2 2) (r2 1 2)
                        , seg' (r2 0 2) (r2 0 1)
                        , seg' (r2 0 0) (r2 2 0)
                        ]
                    expected = [a,b,c]
                in elem (shadowFronts focalPoint segs) (Utils.rotations expected) @?= True

            , testCase "4 disjoint Segs" $
                let
                    focalPoint = r2 1 1
                    segs@[a,b,c,d] = 
                        [ seg' (r2 2 2) (r2 1 2)
                        , seg' (r2 0 2) (r2 0 1)
                        , seg' (r2 0 0) (r2 2 0)
                        , seg' (r2 2 0.1) (r2 2 0.2)
                        ]
                    expected = [a,b,c,d]
                in elem (shadowFronts focalPoint segs) (Utils.rotations expected) @?= True

            , testCase "Overlapping Segs" $
                let
                    focalPoint = r2 1 1
                    segs@[a,b,c] =
                        [ seg' (r2   2     1) (r2   1      2)
                        , seg' (r2   1.5   1) (r2   1.5    2)
                        , seg' (r2 (-2  )  0) (r2 (-2  ) (-2))
                        ]
                    
                    expected = [
                            seg' (r2 1.5 1) (r2 1.5 1.5), seg' (r2 1.5 1.5) (r2 1 2),
                            c
                        ]
                in elem (shadowFronts focalPoint segs) (Utils.rotations expected) @?= True

            , testCase "Doc example origin" $
                let
                    focalPoint = r2 0 0
                    segs@[a,b,c,d,e,f] =
                        [ seg' (r2  (-5)  10) (r2 (-10) 10)
                        , seg' (r2 (-10)  10) (r2 (-10)  5)
                        , seg' (r2 (-10)   5) (r2  (-5)  5)
                        , seg' (r2  (-5)   5) (r2  (-5) 10)
                        , seg' (r2   10  (-1)) (r2   10  1)
                        , seg' (r2   20 (-10)) (r2   20 10)
                        ]
                    expected = [
                            d,
                            c,
                            seg' (r2 20 (-10)) (r2 20 (-2)), e, seg' (r2 20 2) (r2 20 10)
                        ]
                in elem (shadowFronts focalPoint segs) (Utils.rotations expected) @?= True

            , testCase "Doc example" $
                let
                    focalPoint = r2 10 10
                    segs@[a,b,c,d,e,f] =
                        [ seg' (r2  5 20) (r2  0 20)
                        , seg' (r2  0 20) (r2  0 15)
                        , seg' (r2  0 15) (r2  5 15)
                        , seg' (r2  5 15) (r2  5 20)
                        , seg' (r2 20  9) (r2 20 11)
                        , seg' (r2 30  0) (r2 30 20)
                        ]
                    expected = [
                            d,c,
                            seg' (r2 30 0) (r2 30 8), e, seg' (r2 30 12) (r2 30 20)
                        ]
                in elem (shadowFronts focalPoint segs) (Utils.rotations expected) @?= True


            , testCase "Doc example with labels" $
                let
                    focalPoint = r2 10 10
                    segs@[a,b,c,d,e,f] =
                        [ ("a", seg' (r2  5 20) (r2  0 20))
                        , ("b", seg' (r2  0 20) (r2  0 15))
                        , ("c", seg' (r2  0 15) (r2  5 15))
                        , ("d", seg' (r2  5 15) (r2  5 20))
                        , ("e", seg' (r2 20  9) (r2 20 11))
                        , ("f", seg' (r2 30  0) (r2 30 20))
                        ]
                    expected = [
                            d,
                            c,
                            ("f", seg' (r2 30 0) (r2 30 8)), e, ("f", seg' (r2 30 12) (r2 30 20))
                        ]
                in elem (shadowFronts focalPoint segs) (Utils.rotations expected) @?= True
        ]
    ]

atan2Positive y x = ((atan2 y x) + (2*pi)) `mod'` (2*pi)

testsQuickCheck :: TestTree
testsQuickCheck = testGroup "Property Tests" [
    

        testGroup "Angles" [

            testProperty "spreadX2 Ordering the same as atan2" $
                (\(NonZero a@(V2 ax ay)) (NonZero b@(V2 bx (by :: Double)))
                    -> ((atan2Positive ay ax) `compare` (atan2Positive by bx)) === 
                        (unSpr (spreadX2 a) `compare` unSpr (spreadX2 b)))

            , testProperty "isOnSpr Midpoint" $
                (\(v1 :: Rational) -> let a = Spr $ v1 `mod'` 4; mid = Spr $ (v1 + 1) `mod'` 4; b = Spr $ (v1 + 2) `mod'` 4; in isOnSpr mid a b)

            , testProperty "a `isOnSpr` a a" $
                (\(NonZero (v :: V2 TestType)) -> let s = spreadX2 v in isOnSpr s s s)

            , testProperty "a `isOnSpr` a b" $
                (\(NonZero v1) (NonZero (v2 :: V2 TestType)) -> let a = spreadX2 v1; b = spreadX2 v2 in isOnSpr a a b)

            , testProperty "a `isOnSpr` b a" $
                (\(NonZero v1) (NonZero (v2 :: V2 TestType)) -> let a = spreadX2 v1; b = spreadX2 v2 in isOnSpr a b a)

            , testProperty "a /= b /= c ==> (a `isOnSpr` b c /= a `isOnSpr` c b)" $
                (\(NonZero v1) (NonZero v2) (NonZero (v3 :: V2 TestType))
                    -> let a = spreadX2 v1; b = spreadX2 v2; c = spreadX2 v3 in (a /= b && b /= c) ==> (isOnSpr a b c /= isOnSpr a c b))

        ]

        , testGroup "Primitives" [

            testGroup "Line" [

                  testProperty "a == a" $
                    (\(a :: Line TestType) -> a == a)

                , testProperty "Line p d == Line p (d ^* s)    (s /= 0)" $
                    (\p d (NonZero (s :: TestType)) -> line p d == line p (d ^* s))

                , testProperty "Line p d == Line (p + (d ^* s)) d" $
                    (\p d (s :: TestType) -> line p d == line (p + (d ^* s)) d)

                , testProperty "Line p d /= Line (p + dp) d    (dp /= 0 and dp is not colinear with d)" $
                    (\p d (NonZero (dp :: V2 TestType)) -> d `crossZ` dp == 0 || line p d /= line (p + dp) d)

                , testProperty "Line p d /= Line p (d + dd)    (dd /= 0 and dd is not colinear with d)" $
                    (\p d (NonZero (dd :: V2 TestType)) -> d `crossZ` dd == 0 || line p d /= line p (d + dd))
            ]

            , testGroup "Polygon" [

                  testProperty "a == a" $
                    (\(a :: Polygon TestType) -> a == a)

                , testProperty "a == a' where a' could be reversed and rotated a" $
                    (\(a@(Polygon xs) :: Polygon TestType) b c -> a == polygon ((if c then reverse else id) . rotate b $ xs))
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
                (\a b (c :: V2 TestType) -> filterJust $ do
                    s1 <- seg a b
                    s2 <- seg b c
                    return $ segIntersection s1 s2 /= SNothing)

            , testProperty "Colinear Point Intersection" $
                (\p d (Positive lo') (Positive (hi :: TestType))
                    -> let lo = negate lo' in filterJust $ do
                        s  <- seg p (d ^* hi)
                        s1 <- seg p (d ^* lo)
                        s2 <- seg (d ^* lo) p
                        return $ all (== SPoint p) [
                                segIntersection s1 s,
                                segIntersection s2 s
                            ])

            , testProperty "Colinear Sub-Segment Intersection" $
                (\p dir a' b' c' (d' :: TestType)
                    -> let [a,b,c,d] = map (\dp -> p + (dir ^* dp)) $ sort [a',b',c',d'] in
                        filterJust $ do
                            s1 <- seg a c
                            s2 <- seg b c
                            return $ b == c || segIntersection s1 s2 == SSeg s2)

        ]

        , testGroup "Seg Ray Intersect" [

              testProperty "Reverse Ray" $
                (\(seg :: Seg TestType) ray@(Ray p d) -> case segRayIntersection seg ray of
                    SRPoint p' ->
                        p' == p ||
                        segRayIntersection seg (ray' p (negate d)) == SRNothing
                    _ -> True)

            , testProperty "Seg Ray Overlap" $
                (\(p :: V2 TestType) d -> filterJust $ do
                    s <- seg (p + d) (p + 2 *^ d)
                    r <- ray p d
                    return $ segRayIntersection s r == SRSeg s)
        ]

        , testGroup "Point Seg Intersect" [

              testProperty "On First Seg Point" $
                (\s@(Seg a _ :: Seg TestType) -> pointSegIntersection a s == Just a)

            , testProperty "On Second Seg Point" $
                (\s@(Seg _ b :: Seg TestType) -> pointSegIntersection b s == Just b)

            , testProperty "On Midpoint Seg Point" $
                (\s@(Seg a b :: Seg TestType) ->
                    let p = (a + b) ^/ 2
                    in pointSegIntersection p s == Just p)

            , testProperty "seg a a == Nothing" $
                (\(a :: V2 TestType) -> seg a a == Nothing)

            , testProperty "Off Edge" $
                (\s@(Seg a b :: Seg TestType) offsetVector ->
                    -- ensure the offsetVector is not along the segment or at least is far enough to escape the segment.
                    offsetVector `crossZ` (a-b) > 0 || (quadrance offsetVector > quadrance (1-b)) ==>
                    pointSegIntersection (a + offsetVector) s == Nothing)
        ]
    ]

rotate :: Int -> [a] -> [a]
rotate i xs = let l = length xs in take l . drop (mod i l) . cycle $ xs

filterJust :: Testable prop => Maybe prop -> Property
filterJust Nothing  = property Discard
filterJust (Just p) = property p
