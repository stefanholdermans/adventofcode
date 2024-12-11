-- Utilities --

type Bits = [Bool]

encode :: (Integral a) => a -> Bits
encode 0 = []
encode n = odd n : encode (n `div` 2)

decode :: (Integral a) => Bits -> a
decode [] = 0
decode (False : bs) = 2 * decode bs
decode (True : bs) = 2 * decode bs + 1

bits :: (Integral a) => (a -> b) -> Bits -> b
bits f = f . decode

unbits :: (Integral a) => (Bits -> b) -> a -> b
unbits f = f . encode

data Trie a = Trie a (Trie a) (Trie a)

trie :: (Bits -> a) -> Trie a
trie f = Trie (f []) (trie fl) (trie fr)
  where
    fl bs = f (False : bs)
    fr bs = f (True : bs)

untrie :: Trie a -> (Bits -> a)
untrie (Trie x _ _) [] = x
untrie (Trie _ l _) (False : bs) = untrie l bs
untrie (Trie _ _ r) (True : bs) = untrie r bs

memo :: (Integral a) => (a -> b) -> a -> b
memo = unbits . untrie . trie . bits

memo2 :: (Integral a, Integral b) => (a -> b -> c) -> a -> b -> c
memo2 f = memo (memo . f)

-- Parsing --

type Stone = Int

type Stones = [Stone]

parse :: String -> Stones
parse = map read . words

-- Auxiliaries --

evendigs :: Stone -> Bool
evendigs = even . length . show

splitdigs :: Stone -> Stones
splitdigs n = [read ds', read ds'']
  where
    ds = show n
    k = length ds `div` 2
    (ds', ds'') = splitAt k ds

xform :: Stone -> Stones
xform n
  | n == 0 = [1]
  | evendigs n = splitdigs n
  | otherwise = [2024 * n]

blinks :: Int -> Stones -> Int
blinks k = sum . map (blink k)

blink :: Int -> Stone -> Int
blink = memo2 f
  where
    f 0 _ = 1
    f k n = blinks (k - 1) (xform n)

-- Part One --

solve1 :: Stones -> Int
solve1 = blinks 25

-- Part Two --

solve2 :: Stones -> Int
solve2 = blinks 75

-- Entry --

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print (solve1 prob)
  print (solve2 prob)
