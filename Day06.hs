import Data.Array
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set

-- Utilities --

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

-- Parsing --

type Pos = (Int, Int)

data Grid = Grid {cells :: Array Pos Char, start :: Pos}

parse :: String -> Grid
parse input = Grid cells start
  where
    rows@(cols : _) = lines input
    bounds = ((1, 1), (length rows, length cols))
    cells = listArray bounds (concat rows)
    start : _ = [pos | pos <- indices cells, cells ! pos == '^']

-- Auxiliaries --

onGrid :: Pos -> Grid -> Bool
onGrid pos (Grid cells _) = inRange (bounds cells) pos

data Direction = North | East | South | West deriving (Eq, Ord)

turn :: Direction -> Direction
turn North = East
turn East = South
turn South = West
turn West = North

move :: Direction -> Pos -> Pos
move North (i, j) = (i - 1, j)
move East (i, j) = (i, j + 1)
move South (i, j) = (i + 1, j)
move West (i, j) = (i, j - 1)

done :: Grid -> Pos -> Bool
done grid pos = not (pos `onGrid` grid)

clear :: Grid -> Direction -> Pos -> Bool
clear grid dir pos = done grid pos' || cells grid ! pos' /= '#'
  where
    pos' = move dir pos

patrol :: Grid -> Maybe (Set Pos)
patrol grid = go Set.empty North (start grid)
  where
    go visited dir pos
      | done grid pos = Just (Set.map snd visited)
      | Set.member (dir, pos) visited = Nothing
      | clear grid dir pos =
          go (Set.insert (dir, pos) visited) dir (move dir pos)
      | otherwise = go visited (turn dir) pos

-- Part One --

solve1 :: Grid -> Int
solve1 = Set.size . fromJust . patrol

-- Part Two --

obstruct :: Pos -> Grid -> Grid
obstruct pos grid = grid {cells = cells grid // [(pos, '#')]}

solve2 :: Grid -> Int
solve2 grid = count isNothing [patrol (obstruct pos grid) | pos <- visited]
  where
    visited = Set.toList (fromJust (patrol grid))

-- Entry --

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print (solve1 prob)
  print (solve2 prob)
