import Data.List (transpose)

-- Utilities --

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

countUntil :: (a -> Bool) -> [a] -> Int
countUntil p = length . takeWhile (not . p)

-- Parsing --

type Pos = (Int, Int)

type Velocity = (Int, Int)

type Robot = (Pos, Velocity)

type Robots = [Robot]

parse :: String -> Robots
parse = map (pRobot . words) . lines
  where
    pRobot (sp : sv : _) = (pPair sp, pPair sv)
    pPair s = let (sx, _ : sy) = break (== ',') (drop 2 s) in (read sx, read sy)

-- Auxiliaries --

type Bounds = (Int, Int)

simulate :: Bounds -> Robot -> [Pos]
simulate (w, h) (pos, (vx, vy)) = iterate move pos
  where
    move (x, y) = ((x + vx) `mod` w, (y + vy) `mod` h)

-- Part One --

type Region = Pos -> Bool

quadrants :: Bounds -> [Region]
quadrants (w, h) = [q1, q2, q3, q4]
  where
    q1 (x, y) = x < w `div` 2 && y < h `div` 2
    q2 (x, y) = x < w `div` 2 && y > h `div` 2
    q3 (x, y) = x > w `div` 2 && y < h `div` 2
    q4 (x, y) = x > w `div` 2 && y > h `div` 2

solve1 :: Robots -> Int
solve1 rs = product [count q frame | q <- quadrants bounds]
  where
    bounds = (101, 103)
    frame = [simulate bounds r !! 100 | r <- rs]

-- Part Two --

easterEgg :: Bounds -> [Pos] -> Bool
easterEgg (w, _) ps = go ps
  where
    n = 30
    go [] = False
    go ((x, y) : ps')
      | x + n >= w = go ps'
      | otherwise = all (`elem` ps) [(x + dx, y) | dx <- [1 .. n]] || go ps'

solve2 :: Robots -> Int
solve2 = countUntil (easterEgg bounds) . transpose . map (simulate bounds)
  where
    bounds = (101, 103)

-- Entry --

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print (solve1 prob)
  print (solve2 prob)
