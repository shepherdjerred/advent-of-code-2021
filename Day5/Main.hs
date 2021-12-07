{-# LANGUAGE DeriveGeneric #-}
module Main where
-- This solution is incredibly inefficient

import Data.List
import Data.List.Split
import Debug.Trace
import qualified Data.Map as Map
import GHC.Generics (Generic)

data Point = Point { x :: Int, y :: Int } deriving (Show, Eq, Ord, Generic)
type Line = (Point, Point)
type Grid = [Point]
type PointMap = Map.Map Point Int

applyPoint :: Point -> Grid -> Grid
applyPoint point grid = grid ++ [point]

applyHorizontal :: Line -> Grid -> Grid
applyHorizontal = (applyLine' (shorten 1 0))

applyVertical :: Line -> Grid -> Grid
applyVertical = (applyLine' (shorten 0 1))

applyDiagonal :: Line -> Grid -> Grid
applyDiagonal = (applyLine' (shorten 1 1))

applyLine' :: (Line -> Line) -> Line -> Grid -> Grid
applyLine' fn line grid = applyLine (fn line) (applyPoint (fst line) grid)

applyLine :: Line -> Grid -> Grid
applyLine line grid
  | x1 == x2 && y1 == y2 = applyPoint point1 grid
  | y1 == y2 = applyHorizontal line grid
  | x1 == x2 = applyVertical line grid
  | isDiag line = applyDiagonal line grid
  where point1 = fst line
        point2 = snd line
        x1 = x point1
        y1 = y point1
        x2 = x point2
        y2 = y point2

shorten :: Int -> Int -> Line -> Line
shorten x y ((Point x1 y1), (Point x2 y2))
  | x > 0 && y > 0 = shorten (x - 1) (y - 1) (shorten x 0 (shorten 0 y id))
  | x > 0 && x1 > x2 = shorten (x - 1) y ((Point (x1 - 1) y1), (Point x2 y2))
  | x > 0 && x1 < x2 = shorten x y swap
  | x > 0 && x1 == x2 = trace "could not shorten x" $ shorten 0 y id
  | y > 0 && y1 > y2 = shorten x (y - 1) ((Point x1 (y1 - 1)), (Point x2 y2))
  | y > 0 && y1 < y2 = shorten x y swap
  | y > 0 && y1 == y2 = trace "could not shorten y" $ shorten x 0 id
  | x == 0 && y == 0 = id
  where id = ((Point x1 y1), (Point x2 y2))
        swap = ((Point x2 y2), (Point x1 y1))

isDiag :: Line -> Bool
isDiag ((Point x1 y1), (Point x2 y2)) = x1 /= x2 && y1 /= y2

-- Creates a Grid from a list of Lines
populate :: [Line] -> Grid -> Grid
populate [] grid = grid
populate (line:rest) grid = populate rest $ applyLine line grid

readInt :: String -> Int
readInt = read

parsePoint :: String -> Point
parsePoint input = Point (head coords) (last coords)
    where coords = fmap readInt $ splitOn "," input

parseLine :: String -> Line
parseLine input = ((head points), (last points))
    where points = fmap (parsePoint) $ splitOn "->" input

parse :: [String] -> [Line]
parse = fmap parseLine

currentCount :: Point -> PointMap -> Int
currentCount point map = Map.findWithDefault 0 point map

toMap :: Grid -> PointMap
toMap grid = toMap' grid Map.empty

toMap' :: Grid -> PointMap -> PointMap
toMap' [] map = map
toMap' (head:rest) map = toMap' rest $ Map.insert head ((currentCount head map) + 1) map

intersections :: PointMap -> PointMap
intersections map = Map.filter (>= 2) map

main :: IO ()
main = do
    text <- readFile "resources/Day5.txt"
    let input = parse $ lines text
    let notDiag = filter (not . isDiag) input
    let grid = populate notDiag []
    let map = toMap grid
    print $ Map.size $ intersections map
