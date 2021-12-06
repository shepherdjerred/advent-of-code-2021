module Main where

import Data.List
import Data.List.Split

data Point = Point { x :: Int, y :: Int, count :: Int } deriving (Show, Eq)
type Line = (Point, Point)
type Grid = [[(Point)]]

countIntersections :: Int -> Grid -> Int
countIntersections = undefined

applyLine :: Line -> Grid -> Grid
applyAnswer line grid = 

populate :: [Line] -> Grid -> Grid
populate [] grid = grid

readInt :: String -> Int
readInt = read

parsePoint :: String -> Point
parsePoint input = Point (head coords) (last coords) 0
    where coords = fmap readInt $ splitOn "," input

parseLine :: String -> Line
parseLine input = ((head points), (last points))
    where points = fmap (parsePoint) $ splitOn "->" input

parse :: [String] -> [Line]
parse = fmap parseLine

main :: IO ()
main = do
    text <- readFile "resources/Day5.txt"
    let input = parse $ lines text
    print input
