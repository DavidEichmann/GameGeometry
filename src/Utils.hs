module Utils (
          Pos
        , Vec
        , rotate
        , rotations
        , warning
    ) where

import Linear hiding (rotate, trace)
import Data.List
import Debug.Trace (trace)

type Pos = V2
type Vec = V2

rotate :: Int -> [a] -> [a]
rotate i xs = let l = length xs in take l . drop (mod i l) . cycle $ xs

rotations :: [a] -> [[a]]
rotations [] = [[]]
rotations xs = init . (zipWith (++) <$> tails <*> inits) $ xs

warning :: String -> a -> a
warning s = trace ("\x1b[33m\x1b[1mWARNING:\x1b[0m " ++ s)
