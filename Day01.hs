import Data.List (sort)

parse :: String -> ([Int], [Int])
parse s = (map fst pairs, map snd pairs)
  where
    pairs = [(read w1, read w2) | l <- lines s, let [w1, w2] = words l]

distance :: ([Int], [Int]) -> Int
distance (ms, ns) = sum ds
  where
    ds = zipWith (\m n -> abs (m - n)) (sort ms) (sort ns)

similarity :: ([Int], [Int]) -> Int
similarity (ms, ns) = sum ss
  where
    ss = [m * length [n | n <- ns, m == n] | m <- ms]
