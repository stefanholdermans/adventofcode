parse :: String -> [[Int]]
parse = map (map read . words) . lines

safe :: [Int] -> Bool
safe report@(m : n : _) = go report
  where
    p
      | m < n = \i j -> i < j && j - i <= 3
      | otherwise = \i j -> j < i && i - j <= 3

    go (i : j : ks) = p i j && go (j : ks)
    go _ = True

countSafe :: [[Int]] -> Int
countSafe = length . filter safe

dampened :: [Int] -> [[Int]]
dampened [] = [[]]
dampened (i : ks) = ks : map (i :) (dampened ks)

safeWithDampening :: [Int] -> Bool
safeWithDampening = any safe . dampened

countSafeWithDampening :: [[Int]] -> Int
countSafeWithDampening = length . filter safeWithDampening
