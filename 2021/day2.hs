import Data.List
import System.Environment

toTuple :: [String] -> (String, String)
toTuple (x:y:xs) = (x, y)

data PositionOrAim = Position (Int, Int) | PositionAim (Int, Int, Int) deriving (Show)

xPos :: PositionOrAim -> Int
xPos (Position (x, y)) = x
xPos (PositionAim (x, y, a)) = x

yPos :: PositionOrAim -> Int
yPos (Position (x, y)) = y
yPos (PositionAim (x, y, a)) = y

data Command = Forward Int | Up Int | Down Int deriving (Show)

parse :: (String, String) -> Command
parse (c, n)
    | c == "forward" = Forward $ read n
    | c == "up" = Up $ read n
    | c == "down" = Down $ read n

execute :: PositionOrAim -> Command -> PositionOrAim

execute (Position (x, y)) (Forward n) = Position (x + n, y)
execute (Position (x, y)) (Up n) = Position (x, y - n)
execute (Position (x, y)) (Down n) = Position (x, y + n)

execute (PositionAim (x, y, a)) (Forward n) = PositionAim (x + n, y + (a * n), a)
execute (PositionAim (x, y, a)) (Up n) = PositionAim (x, y, a - n)
execute (PositionAim (x, y, a)) (Down n) = PositionAim (x, y, a + n)

main = do
    input <- getContents

    let p = foldl execute (Position (0, 0)) . map (parse . toTuple . words) . lines $ input
    print $ (xPos p) * (yPos p)

    let q = foldl execute (PositionAim (0, 0, 0)) . map (parse . toTuple . words) . lines $ input
    print $ (xPos q) * (yPos q)
