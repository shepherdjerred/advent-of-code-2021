module Main where

import Data.List.Split

simulateDays :: Int -> [Int] -> [Int]
simulateDays days fish
  | days == 0 = fish
  | otherwise = simulateDays (days - 1) (concat $ fmap simulate fish)

simulate :: Int -> [Int]
simulate i
  | i == 0 = [6, 8]
  | otherwise = [i - 1] 

readInt :: String -> Int
readInt = read

parse :: String -> [Int]
parse input = fmap readInt $ splitOn "," input

main :: IO ()
main = do
    text <- readFile "resources/Day6.txt"
    let fish = parse text
    let end = simulateDays 80 fish
    print $ length end
