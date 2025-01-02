{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Bits (shift, xor, (.&.))
import Data.List (tails)
import Data.Map (Map)
import Data.Map qualified as Map (elems, fromListWith, unionsWith)

-- Parsing --

type Secret = Int

type Secrets = [Int]

parse :: String -> Secrets
parse = map read . lines

-- Auxiliaries --

generate :: Int -> Secret -> [Secret]
generate k seed = take (k + 1) ns
  where
    ns = iterate (flip (foldl f) [6, -5, 11]) seed
    f n k = (shift n k `xor` n) .&. mask
    mask = shift 1 24 - 1

-- Part One --

solve1 :: Secrets -> Int
solve1 = sum . map (last . generate 2000)

-- Part Two --

solve2 :: Secrets -> Int
solve2 seeds = maximum (Map.elems proceeds)
  where
    pss = map (map (`mod` 10) . generate 2000) seeds
    wss = map (map (take 4) . tails . changes) pss
    changes ps = zipWith (-) (tail ps) ps
    proceeds = Map.unionsWith (+) (zipWith tabulate pss wss)
    tabulate ps ws = Map.fromListWith (flip const) (zip ws (drop 4 ps))

-- Entry --

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print (solve1 prob)
  print (solve2 prob)
