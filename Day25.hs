import Data.List (transpose)

-- Utilities --

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case break p xs of
  ([], []) -> []
  ([], _ : zs) -> split p zs
  (ys, []) -> [ys]
  (ys, _ : zs) -> ys : split p zs

-- Parsing --

type Height = Int

data Lockbit = Lock [Height] | Key [Height] deriving (Show)

type Lockware = [Lockbit]

parse :: String -> Lockware
parse input = map pLockbit (split null (lines input))
  where
    pLockbit (('#' : _) : ss) = pLock (transpose (init ss))
    pLockbit (('.' : _) : ss) = pKey (transpose (init ss))
    pLock = Lock . map pHeight
    pKey = Key . map (pHeight . reverse)
    pHeight = length . takeWhile (== '#')

-- Part One --

fit :: Lockbit -> Lockbit -> Bool
fit (Lock lhs) (Key khs) = all (<= 5) (zipWith (+) lhs khs)
fit _ _ = False

solve1 :: Lockware -> Int
solve1 lw = length [(lb1, lb2) | lb1 <- lw, lb2 <- lw, fit lb1 lb2]

-- Entry --

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print (solve1 prob)
