import Data.Array

-- Utilities --

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

both :: (a -> Bool) -> (a, a) -> Bool
both p (x, y) = p x && p y

-- Parsing --

type WordSearch = Array Field Char

type Field = (Int, Int)

parse :: String -> WordSearch
parse s = array bounds assocs
  where
    ls@(l : _) = lines s
    bounds = ((0, 0), (length ls - 1, length l - 1))
    assocs = zip (range bounds) (concat ls)

-- Auxiliaries --

type Line = [Field]

valid :: WordSearch -> Line -> Bool
valid = all . inRange . bounds

match :: WordSearch -> String -> Line -> Bool
match ws s l = map (ws !) l == s

-- Part One --

probe :: Field -> [Line]
probe (i, j) = [[(f i k, g j k) | k <- [0 .. 3]] | f <- fs, g <- fs]
  where
    fs = [const, (+), (-)]

solve1 :: WordSearch -> Int
solve1 ws = count matching candidates
  where
    candidates = concatMap (prune . probe) (indices ws)
    prune = filter (valid ws)
    matching = match ws "XMAS"

-- Part Two --

type X = (Line, Line)

probe' :: Field -> [X]
probe' (i, j) = [(l, l') | l <- lines id, l' <- lines negate]
  where
    lines g = [[(f i k, f j (g k)) | k <- [-1 .. 1]] | f <- fs]
    fs = [(+), (-)]

solve2 :: WordSearch -> Int
solve2 ws = count matching candidates
  where
    candidates = concatMap (prune . probe') (indices ws)
    prune = filter (both (valid ws))
    matching = both (match ws "MAS")

-- Entry point --

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print (solve1 prob)
  print (solve2 prob)
