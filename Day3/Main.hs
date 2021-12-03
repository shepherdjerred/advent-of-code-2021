module Main where

import Data.List

-- Converts a binary number represented as a string into a list of individual digits, e.g. "0100" -> [0, 1, 0, 0]
binaryToListOfDigits :: String -> [Int]
binaryToListOfDigits str = map (read . (:"")) str

-- Determines the most frequent bit, e.g. 100 -> 75 -> 1, 100 -> 30 -> 0
mostFrequentBit :: Int -> Int -> Int
mostFrequentBit = mostFrequentBit' undefined

mostFrequentBit' :: Int -> Int -> Int -> Int
mostFrequentBit' favor numberOfBits numberOfHighBits
  | numberOfLowBits < numberOfHighBits = 1
  | numberOfLowBits > numberOfHighBits = 0
  | otherwise = favor
  where numberOfLowBits = numberOfBits - numberOfHighBits

leastFrequentBit :: Int -> Int -> Int -> Int
leastFrequentBit favor numberOfBits numberOfHighBits = (invertBit (mostFrequentBit' favor numberOfBits numberOfHighBits))

invertBit :: Int -> Int
invertBit bit
  | bit == 0 = 1
  | bit == 1 = 0
  | otherwise = undefined

-- Converts a binary number into its decimal representation, e.g. [1, 0, 0] -> 4
binaryToDecimal:: [Int] -> Int
binaryToDecimal = binaryToDecimal' 0

binaryToDecimal' :: Int -> [Int] -> Int
binaryToDecimal' place [] = 0
binaryToDecimal' place list = ((2 ^ place) * (last list)) + (binaryToDecimal' (place + 1) (init list))

matches :: Int -> [Int] -> [[Int]] -> [[Int]]
matches position desired candidates = filter (\x -> ((x!!position) == (desired!!position))) candidates

closestMatch' :: Int -> (Int -> Int -> Int) -> [[Int]] -> [Int]
closestMatch' position mapFn candidates
  | (length currentMatches) == 1 = head currentMatches
  | otherwise = closestMatch' (position + 1) mapFn currentMatches
  where currentMatches = matches position desired candidates
        desired = fmap (mapFn (length candidates)) counts
        counts = fmap sum columns
        columns = transpose candidates

closestMatch :: (Int -> Int -> Int) -> [[Int]] -> [Int]
closestMatch = closestMatch' 0

main :: IO ()
main = do
  text <- readFile "../resources/Day3.txt"
  let values = fmap binaryToListOfDigits $ lines text
  let columns = transpose values
  let counts = fmap sum columns
  print(counts)
  let mostCommonBits = fmap (mostFrequentBit (length values)) counts
  let leastCommonBits = fmap invertBit mostCommonBits
  let gamma = binaryToDecimal mostCommonBits
  let epsilon = binaryToDecimal leastCommonBits
  print("Power Consumption: " ++ (show (gamma * epsilon)))
  let ox = binaryToDecimal (closestMatch (mostFrequentBit' 1) values)
  let co2 = binaryToDecimal (closestMatch (leastFrequentBit 1) values)
  print("Life Support Rating: " ++ (show (ox * co2)))
