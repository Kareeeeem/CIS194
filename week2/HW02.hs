{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Control.Monad (replicateM)

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
  deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches xs ys = length . filter (uncurry (==)) $ zip xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = map f colors
  where f y = length . filter (==y) $ xs

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum . zipWith min (countColors xs) $ countColors ys

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove xs ys = let exact = exactMatches xs ys
                    nonexact = matches xs ys - exact
                 in Move ys exact nonexact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent m@(Move c _ _) c' = m == getMove c' c

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m = filter (isConsistent m)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n = replicateM n colors

-- allCodes :: Int -> [Code]
-- allCodes n
--   | n <= 0 = []
--   | n == 1 = map (:[]) colors
--   | otherwise = concatMap f $ allCodes (n-1)
--     where f code = map (\x -> code ++ [x]) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve c = f (allCodes $ length c)
  where f [] = []
        f (x:xs) = let m = getMove c x
                    in m : f (filterCodes m xs)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
