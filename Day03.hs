import Data.Char (isDigit)

-- Parsing --

type Prog = String

parse :: String -> Prog
parse = id

-- Auxiliaries

data State
  = Mul
  | LParens
  | Multiplier
  | Comma Int
  | Multiplicand Int
  | RParens Int Int

eval :: Prog -> Int
eval = go 0 Mul
  where
    go acc _ [] = acc
    go acc Mul ('m' : 'u' : 'l' : cs) = go acc LParens cs
    go acc LParens ('(' : cs) = go acc Multiplier cs
    go acc Multiplier cs
      | (ds@(_ : _), cs') <- span isDigit cs =
          go acc (Comma (read ds)) cs'
    go acc (Comma m) (',' : cs) = go acc (Multiplicand m) cs
    go acc (Multiplicand m) cs
      | (ds@(_ : _), cs') <- span isDigit cs =
          go acc (RParens m (read ds)) cs'
    go acc (RParens m n) (')' : cs) = go (m * n + acc) Mul cs
    go acc _ (_ : cs) = go acc Mul cs

-- Part One --

solve1 :: Prog -> Int
solve1 = eval

-- Part Two --

preprocess :: Prog -> Prog
preprocess = go True
  where
    go _ [] = []
    go False ('d' : 'o' : '(' : ')' : cs) = go True cs
    go False (_ : cs) = go False cs
    go True ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : cs) = go False cs
    go True (c : cs) = c : go True cs

solve2 :: Prog -> Int
solve2 = eval . preprocess

-- Entry --

main :: IO ()
main = do
  input <- getContents
  let prob = parse input
  print (solve1 prob)
  print (solve2 prob)
