module Utils (
          Pos
        , Vec
        , rotate
    ) where

import Linear hiding (rotate)

type Pos = V2
type Vec = V2

rotate :: Int -> [a] -> [a]
rotate i xs = let l = length xs in take l . drop (mod i l) . cycle $ xs
