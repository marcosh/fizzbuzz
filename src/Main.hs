module Main where

main :: IO ()
main = do
  putStrLn (fizzbuzz 44)

isMultipleOf :: Int -> Int -> Bool
isMultipleOf n domainNumber = mod n domainNumber == 0

isMultipleOfThree :: Int -> Bool
isMultipleOfThree = flip isMultipleOf 3

isMultipleOfFive :: Int -> Bool
isMultipleOfFive = flip isMultipleOf 5

fizzbuzz :: Int -> String
fizzbuzz n
  | isMultipleOfThree n && isMultipleOfFive n = "FizzBuzz"
  | isMultipleOfFive  n                       = "Buzz"
  | isMultipleOfThree n                       = "Fizz"
  | otherwise                                 = show n
