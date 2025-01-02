{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Bits (shift, xor, (.&.))
import Data.List (tails)
import Data.Map (Map)
import Data.Map qualified as Map (elems, fromListWith, unionsWith)

-- Utilities --

pairwiseWith :: (a -> a -> b) -> [a] -> [b]
pairwiseWith f xs = zipWith f xs (tail xs)

windowed :: Int -> [a] -> [[a]]
windowed k xs = [zs | ys <- tails xs, let zs = take k ys, length zs == k]

-- Parsing --

type Secret = Int

type Secrets = [Int]

parse :: String -> Secrets
parse = map read . lines

-- Auxiliaries --

stream :: Secret -> [Secret]
stream = iterate (flip (foldl f) [6, -5, 11])
  where
    f n k = (shift n k `xor` n) .&. mask
    mask = shift 1 24 - 1

-- Part One --

solve1 :: Secrets -> Int
solve1 = sum . map ((!! 2000) . stream)

-- Part Two --

solve2 :: Secrets -> Int
solve2 ns = maximum (Map.elems proceeds)
  where
    pss = map (map (`mod` 10) . stream) ns
    wss = map (windowed 4 . take 2000 . pairwiseWith (flip (-))) pss
    proceeds = Map.unionsWith (+) (zipWith tabulate pss wss)
    tabulate ps ws = Map.fromListWith (flip const) (zip ws (drop 4 ps))

-- Entry --

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print (solve1 prob)
  print (solve2 prob)
