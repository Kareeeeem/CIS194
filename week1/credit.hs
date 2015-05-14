module Main where

toDigits :: Integer  -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:y:xs) = x : y * 2 : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum [ sum (toDigits x) | x <- xs ]

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther ( toDigitsRev x )) `mod` 10 == 0

main :: IO ()
main = do
    putStrLn "Enter your creditcard number."
    x <- getLine
    let y = read x :: Integer
        valid = validate y
    putStrLn (show valid :: String)
