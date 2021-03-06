module Week4 where

import Data.List

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate f
  where f x
         | even x    = x `div` 2
         | otherwise = 3 * x + 1

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)


foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf
  where treeInsert x Leaf = Node 0 Leaf x Leaf
        treeInsert x (Node n l y' r)
          | d l > d r = Node n l y' (treeInsert x r)
          | d l < d r = Node n (treeInsert x l) y' r
          | otherwise = Node m (treeInsert x l) y' r
              where d Leaf = -1
                    d (Node n' _ _ _) = n'
                    m = 1 + max (d r) (d (treeInsert x l))

          -- Alternate solution that decides based on the total count of nodes
          -- left and right.

          -- | c l > c r = Node n l y' (treeInsert x r)
          -- | c l < c r = Node n (treeInsert x l) y' r
          -- | otherwise               = Node m (treeInsert x l) y' r
          --     where d Leaf = -1 -- why -1?
          --           d (Node n' _ _ _) = n'
          --           m = 1 + max (d r) (d (treeInsert x l))
          --           c Leaf = 0
          --           c (Node _ l' _ r') = c l' + 1 + c r'

xor :: [Bool] -> Bool
xor = foldr1 (/=)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x:acc) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram x = map ((+1) . (*2)) ([1..x] \\ sieve)
  where sieve = takeWhile (<=x) [ i+j+2*i*j | i <- [1..], j <- [i..] ]

-- Found this online, should be interesting to study this solution.
-- http://decomputed.com/post/110883687867/sieves-in-haskell-part-1
initialSundaramSieve :: Integral t => t -> [t]
initialSundaramSieve limit =
    let topi = floor (sqrt ((fromIntegral limit) / 2))
     in [i + j + 2*i*j | i <- [1..topi],
     j <- [i..floor((fromIntegral(limit-i)) / fromIntegral(2*i+1))]]

sundaram5 :: Integral a => a -> [a]
sundaram5 limit =
    let halfLimit = (limit `div` 2)
     in 2:removeComposites ([1..halfLimit]) (sort $ initialSundaramSieve halfLimit)

removeComposites :: (Num a, Ord a) => [a] -> [a] -> [a]
removeComposites []     _                  = []
removeComposites sieve  []                 = sieve
removeComposites (s:ss) (c:cs) | s == c    = removeComposites ss cs
  | s > c     = removeComposites (s:ss) cs
  | otherwise = 2*s+1 : (removeComposites ss (c:cs))

