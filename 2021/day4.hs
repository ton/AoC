import Data.Char
import Data.List
import Data.List.Split
import System.Environment

data CardNumber = Unmarked Int | Marked Int deriving (Show)
type Card = [[CardNumber]]

makeCard :: String -> Card
makeCard = map (map (Unmarked . read)) . fmap (wordsBy (not . isDigit)) . wordsBy (\x -> x == '\n')

number :: CardNumber -> Int
number (Unmarked n) = n
number (Marked n) = n

containsMarkedRow :: Card -> Bool
containsMarkedRow = any id . map (all (\x -> case x of {Marked _ -> True; otherwise -> False}))

bingoFound :: Card -> Bool
bingoFound card = containsMarkedRow card || containsMarkedRow (transpose card)

markCard :: Int -> Card -> Card
markCard n = map (foldr (\x acc -> (if number x == n then Marked n else x) : acc) [])

findBingoCard :: [Card] -> Maybe Card
findBingoCard [] = Nothing
findBingoCard (x:xs)
    | bingoFound x = Just x
    | otherwise = findBingoCard xs

markUntilBingo :: [Int] -> [Card] -> Maybe (Card, Int)
markUntilBingo [] _ = Nothing
markUntilBingo _ [] = Nothing
markUntilBingo (x:xs) cards = case bingo of
                                Just card -> Just (card, x)
                                otherwise -> markUntilBingo xs marked
                              where marked = map (markCard x) cards
                                    bingo = findBingoCard marked

findLastBingo :: [Int] -> [Card] -> Maybe (Card, Int)
findLastBingo _ [] = Nothing
findLastBingo [] _ = Nothing
findLastBingo (n:ns) [c]
    | bingoFound marked = Just (marked, n)
    | otherwise = findLastBingo ns [marked]
    where marked = markCard n c
findLastBingo (n:ns) cards = findLastBingo ns (filter (not . bingoFound) . map (markCard n) $ cards)

score :: (Card, Int) -> Int
score (card, n) = (sum . map (foldl (\acc n -> case n of { Unmarked a -> a; otherwise -> 0 } + acc) 0) $ card) * n

main = do
    input <- getContents
    let inputs = splitOn "\n\n" input

    let numbers = map read . wordsBy (not . isDigit) $ head inputs :: [Int]
    let cards = foldr (\cardData acc -> (makeCard cardData) : acc) [] $ tail inputs

    print . fmap score $ markUntilBingo numbers cards
    print . fmap score $ findLastBingo numbers cards
