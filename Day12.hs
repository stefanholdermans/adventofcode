import Data.Array
import Data.List ((\\))

-- Utilities --

summap :: (Num b) => (a -> b) -> [a] -> b
summap f = sum . map f

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

-- Parsing --

type Plant = Char

type Plot = (Int, Int)

type Garden = Array Plot Plant

parse :: String -> Garden
parse input = listArray bounds (concat rows)
  where
    rows@(cols : _) = lines input
    bounds = ((1, 1), (length rows, length cols))

-- Auxiliaries --

valid :: Garden -> Plot -> Bool
valid = inRange . bounds

same :: Garden -> Plot -> Plot -> Bool
same g p p'
  | valid g p && valid g p' = g ! p == g ! p'
  | otherwise = not (valid g p) && not (valid g p')

type Direction = Plot -> Plot

north, east, south, west :: Direction
north (i, j) = (i - 1, j)
east (i, j) = (i, j + 1)
south (i, j) = (i + 1, j)
west (i, j) = (i, j - 1)

directions :: [Direction]
directions = [north, east, south, west]

fence :: Garden -> Plot -> Direction -> Bool
fence g p d = not (same g p (d p))

neighbours :: Garden -> Plot -> [Plot]
neighbours g p = [d p | d <- directions, not (fence g p d)]

type Region = [Plot]

region :: Garden -> Plot -> Region
region g p = go [] [p]
  where
    go acc [] = acc
    go acc (p : ps)
      | p `elem` acc = go acc ps
      | otherwise = go (p : acc) (neighbours g p ++ ps)

regions :: Garden -> [Region]
regions g = go (indices g)
  where
    go [] = []
    go (p : ps) = let r = region g p in r : go (ps \\ r)

-- Part One --

perimeter :: Garden -> Plot -> Int
perimeter g p = count (fence g p) directions

solve1 :: Garden -> Int
solve1 g = summap price (regions g)
  where
    price r = length r * summap (perimeter g) r

-- Part Two --

corners :: Garden -> Plot -> Int
corners g p = count corner intercardinals
  where
    corner (d1, d2) = fence g p d1 && (fence g p d2 || not (fence g (d2 p) d1))
    intercardinals = zip directions (drop 1 (cycle directions))

solve2 :: Garden -> Int
solve2 g = summap price (regions g)
  where
    price r = length r * summap (corners g) r

-- Entry --

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print (solve1 prob)
  print (solve2 prob)
