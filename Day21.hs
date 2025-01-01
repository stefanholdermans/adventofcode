import Data.Array (Array, listArray, range, (!))
import Data.Char (isDigit)
import Data.Maybe (fromJust)

-- Utilities --

lookup' :: (Eq a) => a -> [(a, b)] -> b
lookup' x = fromJust . lookup x

interleavings :: [a] -> [a] -> [[a]]
interleavings [] ys = [ys]
interleavings xs [] = [xs]
interleavings (x : xs) (y : ys) =
  map (x :) (interleavings xs (y : ys)) ++ map (y :) (interleavings (x : xs) ys)

-- Parsing --

type Code = [Char]

type Codes = [Code]

parse :: String -> Codes
parse = lines

-- Auxiliaries --

type Pos = (Int, Int)

path :: Pos -> Code -> [Pos]
path (x, y) [] = [(x, y)]
path (x, y) (c : cs)
  | c == '^' = (x, y) : path (x, y + 1) cs
  | c == 'v' = (x, y) : path (x, y - 1) cs
  | c == '<' = (x, y) : path (x - 1, y) cs
  | c == '>' = (x, y) : path (x + 1, y) cs

type Pad = [(Char, Pos)]

npad :: Pad
npad =
  [ ('0', (1, 0)),
    ('A', (2, 0)),
    ('1', (0, 1)),
    ('2', (1, 1)),
    ('3', (2, 1)),
    ('4', (0, 2)),
    ('5', (1, 2)),
    ('6', (2, 2)),
    ('7', (0, 3)),
    ('8', (1, 3)),
    ('9', (2, 3))
  ]

dpad :: Pad
dpad =
  [ ('<', (0, 0)),
    ('v', (1, 0)),
    ('>', (2, 0)),
    ('^', (1, 1)),
    ('A', (2, 1))
  ]

positions :: Pad -> Code -> [Pos]
positions pad = (lookup' 'A' pad :) . map (flip lookup' pad)

npositions, dpositions :: Code -> [Pos]
npositions = positions npad
dpositions = positions dpad

type Table = Array (Pos, Pos) Int

tabulate :: Pos -> Pos -> (Code -> Int) -> Table
tabulate ubound gap cost = listArray bounds (map compute (range bounds))
  where
    lbound = (0, 0)
    bounds = ((lbound, lbound), (ubound, ubound))
    compute = cheapest gap cost

cheapest :: Pos -> (Code -> Int) -> (Pos, Pos) -> Int
cheapest gap cost ((x1, y1), (x2, y2)) =
  minimum (map (cost . (++ "A")) (filter valid (interleavings cxs cys)))
  where
    dx = x2 - x1
    dy = y2 - y1
    cxs = if dx >= 0 then replicate dx '>' else replicate (-dx) '<'
    cys = if dy >= 0 then replicate dy '^' else replicate (-dy) 'v'
    valid = all (/= gap) . path (x1, y1)

ntabulate, dtabulate :: (Code -> Int) -> Table
ntabulate = tabulate (2, 3) (0, 0)
dtabulate = tabulate (2, 1) (0, 1)

eval :: Table -> [Pos] -> Int
eval t = go
  where
    go (p1 : p2 : ps) = t ! (p1, p2) + go (p2 : ps)
    go _ = 0

neval, deval :: Table -> Code -> Int
neval t = eval t . npositions
deval t = eval t . dpositions

chain :: Int -> Table
chain = ntabulate . cost
  where
    cost 0 = length
    cost k = deval (dtabulate (cost (k - 1)))

complexity :: Int -> Code -> Int
complexity k cs = neval (chain k) cs * read (filter isDigit cs)

-- Part One --

solve1 :: Codes -> Int
solve1 = sum . map (complexity 2)

-- Part Two --

solve2 :: Codes -> Int
solve2 = sum . map (complexity 25)

-- Entry --

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print (solve1 prob)
  print (solve2 prob)
