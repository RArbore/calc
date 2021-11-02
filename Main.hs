{-  This file is part of calc.
    calc is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.
    calc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with calc. If not, see <https://www.gnu.org/licenses/>.  -}

import Data.Maybe
import Data.Char

data Operation = Add | Sub | Mul | Div | Exp deriving (Eq, Ord, Show)
data Token = TokenNumber Integer | TokenOperation Operation | Tokens [Token] deriving (Show)

parseNumberToken :: String -> Maybe (String, Token)
parseNumberToken "" = Nothing
parseNumberToken x
  | digits /= "" = Just (cutDigitHead x, TokenNumber $ read $ digits)
  | otherwise = Nothing
  where digits = takeWhile isDigit x
        cutDigitHead [] = []
        cutDigitHead (y:ys)
          | isDigit(y) = cutDigitHead ys
          | otherwise = y:ys

parseOperationToken :: String -> Maybe (String, Token)
parseOperationToken "" = Nothing
parseOperationToken (x:xs)
  | x == '+' = Just (xs, TokenOperation Add)
  | x == '-' = Just (xs, TokenOperation Sub)
  | x == '*' = Just (xs, TokenOperation Mul)
  | x == '/' = Just (xs, TokenOperation Div)
  | x == '^' = Just (xs, TokenOperation Exp)
  | otherwise = Nothing

parse :: String -> [Token]
parse "" = []
parse (x:xs)
  | isJust numRet = (snd (fromJust numRet)):(parse (fst (fromJust numRet)))
  | isJust opRet = (snd (fromJust opRet)):(parse (fst (fromJust opRet)))
  | x == '(' = Tokens (parse (takeWhile (\c -> c /= ')') xs)):(parse (cutInParens xs))
  | otherwise = parse xs
  where numRet = parseNumberToken (x:xs)
        opRet = parseOperationToken (x:xs)
        cutInParens [] = []
        cutInParens (y:ys)
          | y == ')' = ys
          | otherwise = cutInParens ys

main :: IO ()
main = do
  print $ parse "3 / (5 ^ (3 + 7))"
