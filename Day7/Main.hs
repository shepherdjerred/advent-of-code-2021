module Main where

import Data.List.Split
import Data.List

parse :: String -> [Int]
parse input = fmap read $ splitOn "," input

absolute :: Int -> Int
absolute i
  | i > 0 = i
  | otherwise = -i

-- Finds the median of a sorted list
median :: [Int] -> Int
median nums
  | odd = nums!!midpoint
  | otherwise = ((nums!!midpoint) + (nums!!(midpoint - 1))) `div` 2
  where len = length nums
        odd = len `mod` 2 == 1
        midpoint = len `div` 2

distance :: Int -> Int -> Int
distance median number = absolute(median - number)

main :: IO ()
main = do
  text <- readFile "resources/Day7.txt"
  let crabs = sort $ parse text
  print $ sum $ fmap (distance (median crabs)) crabs
