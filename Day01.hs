import Data.List (sort)

-- Parsing --

type LocationID = Int

type LocationIDs = [LocationID]

type Lists = (LocationIDs, LocationIDs)

parse :: String -> Lists
parse s = (map fst pairs, map snd pairs)
  where
    pairs = [(read w1, read w2) | l <- lines s, let [w1, w2] = words l]

-- Part One --

solve :: Lists -> Int
solve (ms, ns) = sum distances
  where
    distances = zipWith (\m n -> abs (m - n)) (sort ms) (sort ns)

-- Part Two --

solve' :: Lists -> Int
solve' (ms, ns) = sum similarities
  where
    similarities = [m * length [n | n <- ns, m == n] | m <- ms]
