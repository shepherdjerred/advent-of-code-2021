module Main where

data Direction = Up | Down | Forward deriving Show
data Command = Command Direction Int deriving Show
data Position = Position Int Int deriving Show

commandToPosition :: Command -> Position
commandToPosition (Command Up x) = Position 0 (x * (-1))
commandToPosition (Command Down x) = Position 0 x
commandToPosition (Command Forward x) = Position x 0

addPositions :: Position -> Position -> Position
addPositions (Position a b) (Position x y) = Position (a + x) (b + y)

parseDirection :: String -> Direction
parseDirection s 
  | s == "forward" = Forward
  | s == "down" = Down
  | s == "up" = Up
  | otherwise = undefined

readInt :: String -> Int
readInt s = read s

parseCommand :: [String] -> Command
parseCommand [dir, int] = Command (parseDirection dir) (readInt int)

main :: IO ()
main = do
  text <- readFile "resources/Day2.txt"
  let ws = fmap words $ lines text
  let commands = fmap parseCommand ws
  let positions = fmap commandToPosition commands
  let (Position x y) = foldr addPositions (Position 0 0) positions
  print (x * y)
  