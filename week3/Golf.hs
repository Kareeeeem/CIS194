module Golf where

import qualified Data.List as L

skips :: [a] -> [[a]]
skips xs = map (`f` xs) [1..length xs]
  where f n = map snd . filter ((==n) . fst) . zip (cycle [1..n])

-- I actually like this solution too. Seems a bit more readable or no?
skips' :: [a] -> [[a]]
skips' xs = map (f xs) [0..length xs]
  where f ys n
         | n >= length ys  = []
         | otherwise = y : f yx n
          where (y:yx) = drop n ys

localMaxima :: [Integer] -> [Integer]
localMaxima (x:xs@(y:z:_))
  | x < y && y > z = y : localMaxima xs
  | otherwise = localMaxima xs
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs =
    let c = map (\n -> (n, length $ filter (==n) xs)) [0..9]
        m = maximum (map snd c)
     in renderHist . map (histLine m) $ c

renderHist :: [String] -> String
renderHist = L.intercalate "\n" . L.reverse . L.transpose

histLine :: Show a => Int -> (a, Int) -> String
histLine m x = show (fst x) ++ "=" ++
               replicate (snd x) '*' ++
               replicate (m - snd x) ' '

-- Same as the previous but in one funtion
histogram' :: [Integer] -> String
histogram' xs =
        let count  = map (\n -> (n, length $ filter (==n) xs)) [0..9]
            cclock = L.reverse . L.transpose
            m      = maximum . map snd $ count
            f x    = show (fst x) ++ "=" ++
                     replicate (snd x) '*' ++
                     replicate (m - snd x) ' '
         in L.intercalate "\n" . cclock . map f $ count

