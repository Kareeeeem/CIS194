module Week4 where

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

-- This one did my head in. Couldn't get the tree heights right.
foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf
  where treeInsert x Leaf = Node 0 Leaf x Leaf
        treeInsert x (Node n l y' r)
          | d l < d r = Node n (treeInsert x l) y' r
          | d l > d r = Node n l y' (treeInsert x r)
          | otherwise = Node m (treeInsert x r) y' l
              where d Leaf = 0
                    d (Node n' _ _ _) = n'
                    m = d (treeInsert x r) + 1 -- This was needed but why?

xor :: [Bool] -> Bool
xor = foldr1 (/=)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x:acc) []
