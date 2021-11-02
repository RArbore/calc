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

data Operation = Add | Sub | Mul | Div | Exp deriving (Show, Enum, Eq, Bounded)
data Token = TokenNumber Double | TokenOperation Operation | Tokens [Token] deriving (Show)


parse :: String -> [Token]
parse "" = []
parse (x:xs)
  | isJust numRet = (snd (fromJust numRet)):(parse (fst (fromJust numRet)))
  | isJust opRet = (snd (fromJust opRet)):(parse (fst (fromJust opRet)))
  | x == '(' = Tokens (parse (takeWhile (\c -> c /= ')') xs)):(parse (cutInParens xs))
  | otherwise = parse xs
  where opRet = parseOperationToken (x:xs)
          where parseOperationToken "" = Nothing
                parseOperationToken (y:ys)
                  | y == '+' = Just (ys, TokenOperation Add)
                  | y == '-' = Just (ys, TokenOperation Sub)
                  | y == '*' = Just (ys, TokenOperation Mul)
                  | y == '/' = Just (ys, TokenOperation Div)
                  | y == '^' = Just (ys, TokenOperation Exp)
                  | otherwise = Nothing
        numRet = parseNumberToken (x:xs)
          where parseNumberToken "" = Nothing
                parseNumberToken y
                  | digits /= "" = Just (cutDigitHead y, TokenNumber $ read $ digits)
                  | otherwise = Nothing
                  where digits = takeWhile isDigit y
                        cutDigitHead [] = []
                        cutDigitHead (z:zs)
                          | isDigit(z) = cutDigitHead zs
                          | otherwise = z:zs
        cutInParens [] = []
        cutInParens (y:ys)
          | y == ')' = ys
          | otherwise = cutInParens ys

calc :: [Token] -> [Token]
calc x = foldr calcOp x [Add ..]
  where calcOp _ [] = []
        calcOp _ [TokenOperation _] = []
        calcOp _ [TokenNumber a] = [TokenNumber a]
        calcOp op ((Tokens a):xs) = calcOp op ((calc a) ++ xs)
        calcOp op ((TokenNumber a):(TokenOperation o):(Tokens b):xs) = calcOp op ((TokenNumber a):(TokenOperation o):(calc b)) ++ xs
        calcOp op ((TokenNumber a):(TokenOperation o):(TokenNumber b):xs)
          | op == o = calcOp op ((TokenNumber (opCalc a b op)):xs)
          | otherwise = (TokenNumber a):(TokenOperation o):(calcOp op ((TokenNumber b):xs))
            where opCalc oa ob oop
                    | oop == Exp = oa ** ob
                    | oop == Div = oa / ob
                    | oop == Mul = oa * ob
                    | oop == Sub = oa - ob
                    | otherwise = oa + ob
        calcOp _ _ = []

extract :: [Token] -> Maybe Double
extract [TokenNumber a] = Just a
extract _ = Nothing

main :: IO ()
main = do
  print $ extract $ calc $ parse "2*(2*(4 + 1)) / 4 + 7"
