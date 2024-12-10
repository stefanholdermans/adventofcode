import Data.Array
import Data.Set (Set)
import Data.Set qualified as Set

-- Parsing --

type Height = Int

type Grid = [[Int]]

parse :: String -> Grid
parse input = [[read [c] | c <- row] | row <- lines input]

-- Auxiliaries --

type Pos = (Int, Int)

type Path = [Pos]

data Tile = Tile {height :: Height, paths :: Set Path}

type Table = Array Pos Tile

trails :: Table -> [Set Path]
trails tab = [paths t | t <- elems tab, height t == 0]

neighbours :: Table -> Pos -> [Tile]
neighbours tab (i, j) = [tab ! p | p <- ps, inRange (bounds tab) p]
  where
    ps = [(i - 1, j), (i, j - 1), (i, j + 1), (i + 1, j)]

succs :: Table -> Pos -> [Tile]
succs tab p = [t | t <- neighbours tab p, height t == h + 1]
  where
    h = height (tab ! p)

tile :: Table -> Pos -> Height -> Tile
tile _ p 9 = Tile 9 (Set.singleton [p])
tile tab p h = Tile h (Set.unions [Set.map (p :) (paths t) | t <- succs tab p])

tabulate :: Grid -> Table
tabulate rows@(cols : _) = tab
  where
    bounds = ((1, 1), (length rows, length cols))
    tab = array bounds (zipWith assoc (range bounds) (concat rows))
    assoc p h = (p, tile tab p h)

-- Part One --

solve :: Grid -> Int
solve = sum . map score . trails . tabulate
  where
    score = Set.size . Set.map last

-- Part Two --

solve' :: Grid -> Int
solve' = sum . map rating . trails . tabulate
  where
    rating = Set.size
