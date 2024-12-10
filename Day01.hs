import Data.List (sort)

-- Parsing --

type LocationID = Int

type Lists = ([LocationID], [LocationID])

parse :: String -> Lists
parse s = (map fst pairs, map snd pairs)
  where
    pairs = [(read w1, read w2) | l <- lines s, let [w1, w2] = words l]

-- Part One --

solve1 :: Lists -> Int
solve1 (ms, ns) = sum distances
  where
    distances = zipWith (\m n -> abs (m - n)) (sort ms) (sort ns)

-- Part Two --

solve2 :: Lists -> Int
solve2 (ms, ns) = sum similarities
  where
    similarities = [m * length [n | n <- ns, m == n] | m <- ms]

-- Entry --

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print (solve1 prob)
  print (solve2 prob)
