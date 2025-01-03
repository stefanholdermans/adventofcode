{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Maybe (fromJust, isJust)

-- Utilities --

bsearch :: (Int -> Bool) -> Int -> Int
bsearch p n = go 0 (n `div` 2) n
  where
    go i j k
      | k == i + 1 = i
      | p j = go j (j + (k - j) `div` 2) k
      | otherwise = go i (i + (j - i) `div` 2) j

-- Parsing --

type Pos = (Int, Int)

type Bytes = [Pos]

parse :: String -> Bytes
parse = map pPos . lines
  where
    pPos s = read ("(" ++ s ++ ")")

-- Auxiliaires --

neighbours :: Int -> Bytes -> Pos -> [Pos]
neighbours n bytes (x, y) = filter valid xys
  where
    valid xy = inRange xy && xy `notElem` bytes
    inRange (x, y) = x >= 0 && x <= n && y >= 0 && y <= n
    xys = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

shortest :: Int -> Bytes -> Maybe Int
shortest n bytes = go [] [((0, 0), 0)]
  where
    go _ [] = Nothing
    go visited ((xy, d) : q)
      | xy == (n, n) = Just d
      | xy `elem` visited = go visited q
      | otherwise = go (xy : visited) (enq (d + 1) (neighbours n bytes xy) q)
    enq d xys q = q ++ [(xy, d) | xy <- xys]

-- Part One --

solve1 :: Bytes -> Int
solve1 = fromJust . shortest 70 . take 1024

-- Part Two --

solve2 :: Bytes -> Pos
solve2 bytes = bytes !! bsearch reachable (length bytes)
  where
    reachable k = isJust (shortest 70 (take k bytes))

-- Entry --

print' :: (Show a, Show b) => (a, b) -> IO ()
print' = putStrLn . init . tail . show

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print (solve1 prob)
  print' (solve2 prob)
