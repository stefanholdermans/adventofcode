import Data.List (sortBy)

-- Utilities --

breakMap :: (Eq a) => a -> ([a] -> b) -> ([a] -> c) -> [a] -> (b, c)
breakMap x f g xs = (f xs', g xs'')
  where
    (xs', _ : xs'') = break (== x) xs

-- Parsing --

type Rule = (Int, Int)

type Update = [Int]

parse :: String -> ([Rule], [Update])
parse = breakMap "" (map parseR) (map parseU) . lines
  where
    parseR = breakMap '|' read read
    parseU s = read ("[" ++ s ++ "]")

-- Auxiliaries --

validate :: [Rule] -> Update -> Bool
validate rs u = all (check u) rs
  where
    check [] _ = True
    check (k : ks) r@(m, n)
      | k == n = notElem m ks
      | otherwise = check ks r

middle :: Update -> Int
middle u = u !! (length u `div` 2)

-- Part One --

solve :: ([Rule], [Update]) -> Int
solve (rs, us) = sum [middle u | u <- us, validate rs u]

-- Part Two --

ordering :: [Rule] -> Int -> Int -> Ordering
ordering rules i j
  | (i, j) `elem` rules = LT
  | i == j = EQ
  | otherwise = GT

solve' :: ([Rule], [Update]) -> Int
solve' (rs, us) = sum [middle (correct u) | u <- us, not (validate rs u)]
  where
    correct = sortBy (ordering rs)
