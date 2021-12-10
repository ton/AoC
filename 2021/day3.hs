import System.Environment

type Bits = [Bool]

bits2dec :: Bits -> Int
bits2dec = foldl (\acc x -> 2 * acc + fromEnum x) 0

toBits :: String -> Bits
toBits = map (\c -> c == '1')

rate :: (Int -> Bool) -> [Bits] -> Bits
rate fn = map fn . foldl (\acc bits -> zipWith (\x b -> x + fromEnum b) acc bits) (repeat 0)

gamma :: [Bits] -> Bits
gamma xs = rate (\n -> n * 2 >= length xs) xs

epsilon :: [Bits] -> Bits
epsilon xs = rate (\n -> n * 2 < length xs) xs

rating :: ([Bits] -> Bits) -> Int -> [Bits] -> Bits
rating _ _ [] = []
rating _ _ (x:[]) = x
rating criteriumFn bit xs =
    let criterium = criteriumFn xs !! bit
        filtered = filter (\x -> x !! bit == criterium) xs
    in rating criteriumFn (bit + 1) filtered

main = do
    input <- getContents

    let digits = map toBits . lines $ input

    let g = gamma digits
    print $ (bits2dec g) * (bits2dec $ map not g)

    let o = rating gamma 0 digits
    let c = rating epsilon 0 digits
    print $ (bits2dec o) * (bits2dec c)
