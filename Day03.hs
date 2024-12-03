import Data.Char (isDigit)

data State
  = Mul
  | LParens
  | Multiplier
  | Comma Int
  | Multiplicand Int
  | RParens Int Int

parse :: String -> Int
parse = go 0 Mul
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

parseWithConditionals :: String -> Int
parseWithConditionals = go 0 True Mul
  where
    go acc _ _ [] = acc
    go acc False Mul ('d' : 'o' : '(' : ')' : cs) = go acc True Mul cs
    go acc True Mul ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : cs) =
      go acc False Mul cs
    go acc True Mul ('m' : 'u' : 'l' : cs) = go acc True LParens cs
    go acc True LParens ('(' : cs) = go acc True Multiplier cs
    go acc True Multiplier cs
      | (ds@(_ : _), cs') <- span isDigit cs =
          go acc True (Comma (read ds)) cs'
    go acc True (Comma m) (',' : cs) = go acc True (Multiplicand m) cs
    go acc True (Multiplicand m) cs
      | (ds@(_ : _), cs') <- span isDigit cs =
          go acc True (RParens m (read ds)) cs'
    go acc True (RParens m n) (')' : cs) = go (m * n + acc) True Mul cs
    go acc enabled _ (_ : cs) = go acc enabled Mul cs
