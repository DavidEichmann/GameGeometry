
import GameGeometry
import Linear
import Criterion.Main


r2 :: Rational -> Rational -> V2 Rational
r2 = V2

main :: IO ()
main = defaultMain [
        bgroup "Rational Intersection" [

             bench "Line Line Intersection" $ nf (lineIntersection
                                          (Line (r2 0 0) (r2 0 1)))
                                          (Line (r2 2345 2345) (r2 1 2))

           , bench "Seg Seg Intersection" $ nf (segIntersection
                                          (Seg (r2 0 0) (r2 1 1)))
                                          (Seg (r2 1 1) (r2 2 2))
        ]

        , bgroup "Float Line Intersection" [

             bench "Intersection"     $ nf (lineIntersection
                                          (Line (V2    0    0) (V2 0  1)))
                                          (Line (V2 2345 2345) (V2 1 (2 :: Float)))

           , bench "No Intersection"  $ nf (lineIntersection
                                          (Line (V2 0 0) (V2 1 1)))
                                          (Line (V2 2345 2345) (V2 1 (1 :: Float)))
        ]
    ]
