import Data.List.Split
import qualified Data.Map as Map
import System.Environment

type Point = (Int, Int)
type Line = (Point, Point)
type Grid = Map.Map (Int, Int) Int

toTuple :: [a] -> (a, a)
toTuple (x:y:xs) = (x, y)

parsePoint :: String -> Point
parsePoint = toTuple . map read . wordsBy (== ',')

parseLine :: String -> Line
parseLine = toTuple . map parsePoint . splitOn "->"

fromTo :: (Ord a, Enum a) => a -> a -> [a]
fromTo x y
    | x < y     = [x..y]
    | otherwise = [x, pred x..y]

linePoints :: Line -> [Point]
linePoints ((a, b), (c, d))
    | a == c = zip (repeat a) (fromTo b d)
    | b == d = zip (fromTo a c) (repeat b)
    | otherwise = zip (fromTo a c) (fromTo b d) -- diagonals; use `otherwise = []` for part A

addPointToGrid :: Point -> Grid -> Grid
addPointToGrid p = Map.insertWith (+) p 1

addLineToGrid :: Line -> Grid -> Grid
addLineToGrid l g = foldl (\acc p -> addPointToGrid p acc) g (linePoints l)

buildGrid :: [Line] -> Grid
buildGrid = foldl (\acc l -> addLineToGrid l acc) Map.empty

numOverlaps :: Int -> Grid -> Int
numOverlaps n = Map.foldr (\x acc -> if n <= x then acc + 1 else acc) 0

main = do
    grid <- buildGrid . map parseLine . lines <$> getContents
    print $ numOverlaps 2 grid
