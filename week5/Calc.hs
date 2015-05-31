module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- evalStr :: String -> Maybe Integer
-- evalStr s
--   | s' == Nothing = Nothing
--   | otherwise = Just . eval . fromJust $ s'
--     where s' = parseExp Lit Add Mul s

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
              Just s' -> Just $ eval s'
              Nothing -> Nothing
