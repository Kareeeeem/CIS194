module Golf where

import qualified Data.List as L
-- import qualified Data.Maybe as M

skips :: [a] -> [[a]]
skips xs = map (`f` xs) [1..length xs]
  where f n = map snd . filter ((==n) . fst) . zip (cycle [1..n])

  -- where f ys n
  --        | n >= length ys  = []
  --        | otherwise = y : f yx n
  --         where (y:yx) = drop n ys

localMaxima :: [Integer] -> [Integer]
localMaxima (x:xs@(y:z:_))
  | x < y && y > z = y : localMaxima xs
  | otherwise = localMaxima xs
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs =
    let a x = length . filter (==x) $ xs
        m = maximum $ map length . L.group . L.sort $ xs
        f x = show x  ++ "=" ++ replicate (a x) '*' ++ replicate (m - a x) ' '
     in L.intercalate "\n" (L.reverse . L.transpose . map f $ [0..9])

histogram' :: [Integer] -> String
histogram' xs =
    let c = map (\n -> (n, length $ filter (==n) xs)) [0..9]
        m = maximum . map snd $ c
        f x = show (fst x) ++ "=" ++
          replicate (snd x) '*' ++
          replicate (m - snd x) ' '
     in L.intercalate "\n" (counterClock . map f $ c)
       where counterClock = L.reverse . L.transpose

-- histogram :: [Integer] -> String
-- histogram xs =
--     let s = map (\l@(x:_) -> (x, length l)) . L.group . L.sort $ xs
--         m = maximum . map snd $ s
--         a x = L.find ((==x).fst) s
--         f x = show x  ++ "=" ++ padR m (score (a x))
--      in L.intercalate "\n" (counterClock . map f $ [0..9])
--        where counterClock = L.reverse . L.transpose

-- score :: M.Maybe (Integer, Int) -> String
-- score Nothing = ""
-- score (Just (_, y)) = replicate y '*'

-- padR :: Int -> String -> String
-- padR n s = s ++ replicate (n - length s) ' '
