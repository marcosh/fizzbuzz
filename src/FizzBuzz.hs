module FizzBuzz where

-- containers
import qualified Data.Map.Lazy as Map (Map, findWithDefault, foldrWithKey, fromList)

main :: IO ()
main = do
  print $ take 50 (fizzBuzzGenList config [1 ..])

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

fizzbuzzList :: [Int] -> [String]
fizzbuzzList = map fizzbuzz

--- allow to specify numbers and strings

fizzBuzzGen :: Map.Map Int String -> Int -> String
fizzBuzzGen config n =
  let
    acc :: Int -> String -> Maybe String -> Maybe String
    acc key value Nothing  = if n `isMultipleOf` key then Just value else Nothing
    acc key value (Just s) = if n `isMultipleOf` key then Just (value <> s) else Just s

    possiblyEmptyString = Map.foldrWithKey acc Nothing config

  in case possiblyEmptyString of
    Nothing -> show n
    Just s  -> s

fizzBuzzGenList :: Map.Map Int String -> [Int] -> [String]
fizzBuzzGenList config = map (fizzBuzzGen config)

config :: Map.Map Int String
config = Map.fromList [(3, "Fizz"), (5,"Buzz")]
