-- Parsing --

type Equation = (Int, [Int])

type Equations = [Equation]

parse :: String -> Equations
parse = map (parseEq . words) . lines
  where
    parseEq (s : ss) = (read (init s), map read ss)

-- Auxiliaries --

solvable :: [Int -> Int -> Int] -> Equation -> Bool
solvable ops (m, n : ns) = go n ns
  where
    go acc [] = acc == m
    go acc (n : ns)
      | acc > m = False
      | otherwise = or [go (acc `op` n) ns | op <- ops]

calibrate :: [Int -> Int -> Int] -> Equations -> Int
calibrate ops = sum . map fst . filter (solvable ops)

-- Part One --

solve1 :: Equations -> Int
solve1 = calibrate [(+), (*)]

-- Part Two --

(><) :: Int -> Int -> Int
m >< n = read (show m ++ show n)

solve2 :: Equations -> Int
solve2 = calibrate [(+), (*), (><)]

-- Entry point --

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print (solve1 prob)
  print (solve2 prob)
