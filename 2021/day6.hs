import Data.List
import Data.List.Split
import System.Environment

type Histogram = [Int]

add :: Int -> Int -> Histogram -> Histogram
add 0 n [] = [n]
add 0 n (x:xs) = (n + x):xs
add i n [] = 0:add (i - 1) n []
add i n (x:xs) = x:add (i - 1) n xs

type LanternFish = Histogram -- Count of fish per day life-cycle

makeLanternFish :: String -> LanternFish
makeLanternFish = foldl classify [] . group . sort . map read . wordsBy (== ',')
                  where classify h xs = add (head xs) (length xs) h

data SpawnInfo = SpawnInfo { spawnAfter :: Int
                           , adultAfter :: Int } deriving (Show)

reproduceWith :: SpawnInfo -> LanternFish -> LanternFish
reproduceWith si [] = []
reproduceWith si (numAdults:children) = add (spawnAfter si) numAdults . add (adultAfter si) numAdults $ children

main = do
    input <- getLine

    let lanternFish = makeLanternFish input

    let adultAfter = 8
    let spawnAfter = 6
    let reproduce = reproduceWith $ SpawnInfo spawnAfter adultAfter

    let days = 256

    print . sum $ iterate reproduce lanternFish !! days
