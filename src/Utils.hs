module Utils (
          Pos
        , Vec
        , rotate
        , rotations
    ) where

import Linear hiding (rotate)
import Data.List

type Pos = V2
type Vec = V2

rotate :: Int -> [a] -> [a]
rotate i xs = let l = length xs in take l . drop (mod i l) . cycle $ xs

rotations :: [a] -> [[a]]
rotations [] = [[]]
rotations xs = init . (zipWith (++) <$> tails <*> inits) $ xs
