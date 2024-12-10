-- Utilities --

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

-- Parsing --

type Level = Int

type Report = [Level]

type Reports = [Report]

parse :: String -> Reports
parse = map (map read . words) . lines

--  Auxiliaries --

safe :: Report -> Bool
safe report@(m : n : _) = go report
  where
    p
      | m < n = \i j -> i < j && j - i <= 3
      | otherwise = \i j -> j < i && i - j <= 3

    go (i : j : ks) = p i j && go (j : ks)
    go _ = True

-- Part One --

solve1 :: Reports -> Int
solve1 = count safe

-- Part Two --

dampened :: Report -> [Report]
dampened [] = [[]]
dampened (i : ks) = ks : map (i :) (dampened ks)

solve2 :: Reports -> Int
solve2 = count (any safe . dampened)

-- Entry --

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print (solve1 prob)
  print (solve2 prob)
