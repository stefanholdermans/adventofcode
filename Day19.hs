import Data.Array (Array, listArray, (!))
import Data.List (isPrefixOf)
import Data.Monoid (Any (Any), Sum (Sum), getAny, getSum)

-- Utilities --

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

sumOn :: (Num b) => (a -> b) -> [a] -> b
sumOn f = sum . map f

-- Parsing --

type Pattern = [Char]

type Design = [Char]

type Universe = ([Pattern], [Design])

parse :: String -> Universe
parse input = (ps, ds)
  where
    pcs : _ : ds = lines input
    ps = map init (words (pcs ++ ","))

-- Auxiliaries --

arrangements :: (Monoid a) => a -> [Pattern] -> Design -> a
arrangements x ps d = xs ! 0
  where
    n = length d
    xs = listArray (0, n) (map compute [0 .. n])
    compute k = if k == n then x else foldMap (match k) ps
    match k p = if p `isPrefixOf` drop k d then xs ! (k + length p) else mempty

-- Part One --

possible :: [Pattern] -> Design -> Bool
possible ps = getAny . arrangements (Any True) ps

solve1 :: Universe -> Int
solve1 (ps, ds) = count (possible ps) ds

-- Part Two --

ways :: [Pattern] -> Design -> Int
ways ps = getSum . arrangements (Sum 1) ps

solve2 :: Universe -> Int
solve2 (ps, ds) = sumOn (ways ps) ds

-- Entry --

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print (solve1 prob)
  print (solve2 prob)
