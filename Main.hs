import Data.Char

data Operation = Add | Sub | Mul | Div | Exp deriving (Eq, Ord)
data Token = TokenNumber Integer | TokenOperation | Tokens [Token] deriving (Show)

parseNumberToken :: String -> Maybe (String, Token)
parseNumberToken "" = Nothing
parseNumberToken x
  | digits /= "" = Just (cutDigitHead x, TokenNumber $ read $ digits)
  | otherwise = Nothing
  where digits = takeWhile isDigit x
        cutDigitHead (x:xs)
          | isDigit(x) = cutDigitHead(xs)
          | otherwise = x:xs

main :: IO ()
main = do
  print $ parseNumberToken "43 5 "
