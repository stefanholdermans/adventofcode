import Data.Ix (inRange, range)
import Data.List (nub)
import Data.Map qualified as Map (elems, fromListWith)

-- Utilities --

unique :: (Eq a) => [a] -> Int
unique = length . nub

-- Parsing --

type Loc = (Int, Int)

type Freq = [Loc]

data Grid = Grid {bounds :: (Loc, Loc), freqs :: [Freq]}

parse :: String -> Grid
parse input = Grid bounds freqs
  where
    rows@(cols : _) = lines input
    bounds = ((1, 1), (length rows, length cols))
    cells = zip (concat rows) (range bounds)
    antennas = Map.fromListWith (++) [(c, [pos]) | (c, pos) <- cells, c /= '.']
    freqs = Map.elems antennas

-- Auxiliaries --

type Model = [Int]

anti :: Model -> (Loc, Loc) -> [Loc]
anti model ((i, j), (i', j')) = [(i + k * di, j + k * dj) | k <- model]
  where
    di = i' - i
    dj = j' - j

impact :: Model -> Grid -> Int
impact model (Grid bounds freqs) = unique locs
  where
    locs = concatMap (prune . anti model) (concatMap pairs freqs)
    pairs freq = [(loc, loc') | loc <- freq, loc' <- freq, loc /= loc']
    prune = takeWhile (inRange bounds)

-- Part One --

solve :: Grid -> Int
solve = impact [2]

-- Part Two --

solve' :: Grid -> Int
solve' = impact [1 ..]
