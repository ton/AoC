import Data.List
import System.Environment

pairs :: [a] -> [(a, a)]
pairs (x:y:xs) = (x, y):(pairs $ y:xs)
pairs _ = []

triplets :: [a] -> [[a]]
triplets (x:y:z:xs) = [x, y, z]:(triplets $ y:z:xs)
triplets _ = []

numIncreasing :: (Ord a) => [(a, a)] -> Int
numIncreasing xs = sum . map (uncurry (\x y -> if x < y then 1 else 0)) $ xs

main = do
    input <- getContents

    let depths = map read $ lines input :: [Int]
    let depthPairs = pairs depths
    let depthTriplets = triplets depths

    print $ numIncreasing depthPairs
    print $ numIncreasing . pairs . map sum $ depthTriplets
