import Data.List
import Data.List.Split
import System.Environment

type Histogram = [Int]

add :: Int -> Int -> Histogram -> Histogram
add 0 n [] = [n]
add 0 n (x:xs) = (n + x):xs
add i n [] = 0:add (i - 1) n []
add i n (x:xs) = x:add (i - 1) n xs

makeHistogram :: String -> Histogram
makeHistogram = foldl classify [] . group . sort . map read . wordsBy (== ',')
                where classify h xs = add (head xs) (length xs) h

fuelCostA :: Int -> Histogram -> Int
fuelCostA pos [] = 0
fuelCostA pos (x:xs) = (x * (abs pos)) + (fuelCostA (pos - 1) xs)

fuelCostB :: Int -> Histogram -> Int
fuelCostB pos [] = 0
fuelCostB pos (x:xs) = x * ((n * (n + 1)) `div` 2) + (fuelCostB (pos - 1) xs)
                       where n = abs pos

minimumFuel :: [Int] -> Int
minimumFuel [] = 0
minimumFuel (x:y:xs)
    | x >= y = minimumFuel (y:xs)
    | x < y = x

main = do
    h <- makeHistogram <$> getLine

    print . minimumFuel $ map (flip fuelCostA h) [0..length h]
    print . minimumFuel $ map (flip fuelCostB h) [0..length h]
