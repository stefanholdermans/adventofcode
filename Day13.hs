-- Utilities --

chunk :: Int -> [a] -> [[a]]
chunk n = go
  where
    go [] = []
    go xs = let (ys, zs) = splitAt n xs in ys : go zs

-- Parsing --

type Button = (Int, Int)

type Prize = (Int, Int)

type Machine = (Button, Button, Prize)

type Machines = [Machine]

parse :: String -> Machines
parse = map pMachine . chunk 4 . map words . lines
  where
    pMachine (ssa : ssb : ssp : _) = (pButton ssa, pButton ssb, pPrize ssp)
    pButton (_ : _ : sx : sy : _) = pPair sx sy
    pPrize (_ : sx : sy : _) = pPair sx sy
    pPair sx sy = (read (drop 2 (init sx)), read (drop 2 sy))

-- Auxiliaries --

solve :: Machine -> Int
solve ((dxa, dya), (dxb, dyb), (x, y))
  | deta `mod` det /= 0 = 0
  | detb `mod` det /= 0 = 0
  | otherwise = 3 * (deta `div` det) + (detb `div` det)
  where
    det = dxa * dyb - dxb * dya
    deta = x * dyb - dxb * y
    detb = dxa * y - x * dya

-- Part One --

solve1 :: Machines -> Int
solve1 = sum . map solve

-- Part Two --

solve2 :: Machines -> Int
solve2 = sum . map (solve . correct)
  where
    correct (da, db, (x, y)) = (da, db, (x + d, y + d))
    d = 10000000000000

-- Entry --

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print (solve1 prob)
  print (solve2 prob)
