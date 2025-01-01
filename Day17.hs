{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Bits (shiftL, shiftR, xor)

-- Parsing --

type Regs = (Int, Int, Int)

type Prog = [Int]

type Machine = (Prog, Regs)

parse :: String -> Machine
parse = dump . map words . lines
  where
    dump [[_, _, sa], [_, _, sb], [_, _, sc], _, [_, sp]] =
      (read ("[" ++ sp ++ "]"), ((read sa, read sb, read sc)))

-- Part One --

combo :: Int -> Regs -> Int
combo 4 (ax, _, _) = ax
combo 5 (_, bx, _) = bx
combo 6 (_, _, cx) = cx
combo k _ = k

run :: Prog -> Regs -> [Int]
run prog = reverse . go [] 0
  where
    go acc ip _ | ip >= length prog = acc
    go acc ip regs@(ax, bx, cx) = case (prog !! ip, prog !! (ip + 1)) of
      (0, k) ->
        let n = combo k regs
            ax' = shiftR ax n
         in go acc (ip + 2) (ax', bx, cx)
      (1, n) ->
        let bx' = xor bx n
         in go acc (ip + 2) (ax, bx', cx)
      (2, k) ->
        let n = combo k regs
            bx' = n `mod` 8
         in go acc (ip + 2) (ax, bx', cx)
      (3, n) ->
        let ip' = if ax == 0 then ip + 2 else n
         in go acc ip' regs
      (4, _) ->
        let bx' = xor bx cx
         in go acc (ip + 2) (ax, bx', cx)
      (5, k) ->
        let n = combo k regs
            m = n `mod` 8
         in go (m : acc) (ip + 2) regs
      (6, k) ->
        let n = combo k regs
            bx' = shiftR ax n
         in go acc (ip + 2) (ax, bx', cx)
      (7, k) ->
        let n = combo k regs
            cx' = shiftR ax n
         in go acc (ip + 2) (ax, bx, cx')

solve1 :: Machine -> [Int]
solve1 = uncurry run

-- Part Two --

solve2 :: Machine -> Int
solve2 (prog, (_, bx, cx)) = go 0 (length prog - 1)
  where
    go ax m
      | run prog (ax, bx, cx) /= drop m prog = go (ax + 1) m
      | m /= 0 = go (shiftL ax 3) (m - 1)
      | otherwise = ax

-- Entry --

print' :: (Show a) => [a] -> IO ()
print' = putStrLn . init . tail . show

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print' (solve1 prob)
  print (solve2 prob)
