module Main where

import Data.List

-- Converts a binary number represented as a string into a list of individual digits, e.g. "0100" -> [0, 1, 0, 0]
binaryToListOfDigits :: String -> [Int]
binaryToListOfDigits = map $ read . (:"")

-- Determines the most frequent bit, e.g. 100 -> 75 -> 1, 100 -> 30 -> 0
mostFrequentBit :: Int -> Int -> Int -> Int
mostFrequentBit favoredBit numberOfBits numberOfHighBits
  | numberOfLowBits < numberOfHighBits = 1
  | numberOfLowBits > numberOfHighBits = 0
  | otherwise = favoredBit
  where numberOfLowBits = numberOfBits - numberOfHighBits

leastFrequentBit :: Int -> Int -> Int -> Int
leastFrequentBit = ((.) . (.) . (.)) invertBit mostFrequentBit

invertBit :: Int -> Int
invertBit bit
  | bit == 0 = 1
  | bit == 1 = 0
  | otherwise = undefined

-- Converts a binary number into its decimal representation, e.g. [1, 0, 0] -> 4
binaryToDecimal:: [Int] -> Int
binaryToDecimal = binaryToDecimal' 0

binaryToDecimal' :: Int -> [Int] -> Int
binaryToDecimal' power [] = 0
binaryToDecimal' power list = 2 ^ power * (last list) + binaryToDecimal' (power + 1) (init list)

-- Determines if two arrays are equal at a given index
doesMatch :: Int -> [Int] -> [Int] -> Bool
doesMatch position current desired = current!!position == desired!!position

-- Finds all subarrays that match a desired array at a given index
allMatches :: Int -> [Int] -> [[Int]] -> [[Int]]
allMatches position desired candidates = filter (doesMatch position desired) candidates

closestMatch :: (Int -> Int -> Int) -> [[Int]] -> [Int]
closestMatch = closestMatch' 0

closestMatch' :: Int -> (Int -> Int -> Int) -> [[Int]] -> [Int]
closestMatch' position fn candidates
  | length currentMatches == 0 = undefined
  | length currentMatches == 1 = head currentMatches
  | otherwise = closestMatch' (position + 1) fn currentMatches
  where currentMatches = allMatches position desired candidates
        desired = fmap (fn $ length candidates) counts
        counts = fmap sum columns
        columns = transpose candidates

main :: IO ()
main = do
  text <- readFile "resources/Day3.txt"
  let values = fmap binaryToListOfDigits $ lines text
  let mostCommonBits = fmap (mostFrequentBit undefined (length values)) $ fmap sum $ transpose values
  let gamma = binaryToDecimal mostCommonBits
  let epsilon = binaryToDecimal $ fmap invertBit mostCommonBits
  print("Power Consumption: " ++ (show (gamma * epsilon)))
  let ox = binaryToDecimal $ closestMatch (mostFrequentBit 1) values
  let co2 = binaryToDecimal $ closestMatch (leastFrequentBit 1) values
  print("Life Support Rating: " ++ (show (ox * co2)))
