-- Parsing --

type Size = Int

type ID = Int

data Frag = File {size :: Size, ident :: ID} | Free {size :: Size}

type DiskMap = [Frag]

parse :: String -> DiskMap
parse = go True 0
  where
    go _ _ [] = []
    go file i (c : cs)
      | file = File (read [c]) i : go False (i + 1) cs
      | otherwise = Free (read [c]) : go True i cs

-- Auxiliaries --

checksum :: DiskMap -> Int
checksum = sum . zipWith (*) [0 ..] . concatMap f
  where
    f (File n i) = replicate n i
    f (Free n) = replicate n 0

-- Part One --

resize :: Size -> Frag -> Frag
resize m (File _ i) = File m i
resize m (Free _) = Free m

defrag :: DiskMap -> DiskMap
defrag frags = go (sum (map size frags)) frags (reverse frags)
  where
    go 0 _ _ = []
    go k (frag : frags) frags' | k < size frag = go k [resize k frag] frags'
    go k frags (frag' : frags') | k < size frag' = go k frags [resize k frag']
    go k (frag@(File m _) : frags) frags' = frag : go (k - m) frags frags'
    go k frags (Free n : frags') = go (k - n) frags frags'
    go k (Free m : frags) (File n j : frags')
      | m < n = File m j : go (k - 2 * m) frags (File (n - m) j : frags')
      | m == n = File n j : go (k - 2 * m) frags frags'
      | otherwise = File n j : go (k - 2 * n) (Free (m - n) : frags) frags'

solve :: DiskMap -> Int
solve = checksum . defrag

-- Part Two --

pad :: Size -> [Frag] -> [Frag]
pad 0 frags = frags
pad m (Free n : frags) = Free (m + n) : frags
pad m frags = Free m : frags

insert :: Size -> ID -> [Frag] -> Maybe [Frag]
insert m i = go
  where
    go [] = Nothing
    go (frag@(Free n) : frags) | m <= n = case go frags of
      Nothing -> Just (pad (n - m) (File m i : frags))
      Just frags' -> Just (frag : frags')
    go (frag : frags) = fmap (frag :) (go frags)

defrag' :: DiskMap -> DiskMap
defrag' = reverse . go . reverse
  where
    go [] = []
    go (frag@(File n i) : frags) = case insert n i frags of
      Nothing -> frag : go frags
      Just frags' -> pad n (go frags')
    go (Free n : frags) = pad n (go frags)

solve' :: DiskMap -> Int
solve' = checksum . defrag'
