module Main where

import Data.List

-- Converts a binary number represented as a string into a list of individual digits, e.g. "0100" -> [0, 1, 0, 0]
binaryToListOfDigits :: String -> [Int]
binaryToListOfDigits str = map (read . (:"")) str

-- Determines the most frequent bit, e.g. 100 -> 75 -> 1, 100 -> 30 -> 0
mostFrequentBit :: Int -> Int -> Int
mostFrequentBit numberOfBits numberOfHighBits
  | numberOfLowBits < numberOfHighBits = 1
  | numberOfLowBits > numberOfHighBits = 0
  | otherwise = undefined
  where numberOfLowBits = numberOfBits - numberOfHighBits

invertBit :: Int -> Int
invertBit bit
  | bit == 0 = 1
  | bit == 1 = 0
  | otherwise = undefined

-- Converts a binary number into its decimal representation, e.g. [1, 0, 0] -> 4
binaryToDecimal:: [Int] -> Int
binaryToDecimal i = binaryToDecimal' 0 i

binaryToDecimal' :: Int -> [Int] -> Int
binaryToDecimal' place [] = 0
binaryToDecimal' place list = ((2 ^ place) * (last list)) + (binaryToDecimal' (place + 1) (init list))

main :: IO ()
main = do
  text <- readFile "../resources/Day3.txt"
  let values = fmap binaryToListOfDigits $ lines text
  let columns = transpose values
  let counts = fmap sum columns
  let mostCommonBits = fmap (mostFrequentBit (length values)) counts
  let leastCommonBits = fmap invertBit mostCommonBits
  let gamma = binaryToDecimal mostCommonBits
  let epsilon = binaryToDecimal leastCommonBits
  print("Power Consumption: " ++ gamma * epsilon)
