module Main where

import Data.List (sortBy, sort)

type Card = [Int]
type Bet = Int

data Game = Game Card Bet deriving(Show)


parseLine:: String -> Game
parseLine s = Game c b
  where
    wds = words s
    c = stringToCard $ head wds
    b = read $ head $ tail wds :: Bet

stringToCard:: String -> Card
stringToCard (s:ss)
  | null (s:ss) = []
  | s == '2' = 1: stringToCard ss
  | s == '3' = 2: stringToCard ss
  | s == '4' = 3: stringToCard ss
  | s == '5' = 4: stringToCard ss
  | s == '6' = 5: stringToCard ss
  | s == '7' = 6: stringToCard ss
  | s == '8' = 7: stringToCard ss
  | s == '9' = 8: stringToCard ss
  | s == 'T' = 9: stringToCard ss
  | s == 'J' = 0: stringToCard ss -- weakest in direct challenge
  | s == 'Q' = 11: stringToCard ss
  | s == 'K' = 12: stringToCard ss
  | s == 'A' = 13: stringToCard ss
stringToCard _ = []

compareCards:: Card -> Card -> Ordering
compareCards c d
    | rank c > rank d = GT
    | rank c < rank d = LT
    | otherwise       = compareFromFirst c d

rank:: Card -> Int
rank c
  | head cp == 5 = 10 -- 5 of a kind
  | head cp == 4 = 9 --4 of a kind
  | length cp == 2 && (head cp == 3) && ( cp !! 1  == 2) = 8 -- full house
  | elem 3 cp = 7
  | elem 2 cp && countCopiesOf cp 2 == 2 = 4
  | elem 2 cp = 2
  | otherwise = 1
  where
    cpwoJ = reverse $ sort $ countCopies $ removeA c 0
    jollys = countCopiesOf c 0
    cp = if not (null cpwoJ)
      then jollys + head cpwoJ : tail cpwoJ
      else [jollys]

countCopies:: [Int] -> [Int]
countCopies [] = []
countCopies (a:as) = (1 + countCopiesOf as a):countCopies (removeA as a)

countCopiesOf:: [Int] -> Int -> Int
countCopiesOf [] _ = 0
countCopiesOf (c:cs) a
  | a == c = 1 + countCopiesOf cs a
  | otherwise = countCopiesOf cs a

removeA:: [Int] -> Int -> [Int]
removeA [] _ = []
removeA (x:xs) a
  | x == a = removeA xs a
  | otherwise = x:removeA xs a

compareFromFirst:: Card -> Card -> Ordering
compareFromFirst [] [] = EQ
compareFromFirst _ [] = EQ
compareFromFirst [] _ = EQ
compareFromFirst (c:cs) (d:ds)
  | c > d = GT
  | c < d =  LT
  | otherwise = compareFromFirst cs ds


main :: IO ()
main = do
  rawfile <- readFile "input.txt"
  let lns = lines rawfile
  let games = map parseLine lns
  let sorted = sortBy (\ (Game a1 _)  (Game a2 _) -> compareCards a1 a2) games
  let finalScoreForGames = reverse $ zip [x | Game _ x <- sorted][1..length games]
  print $ foldl (+) 0 $ map (\(x, y) -> (x * y)) finalScoreForGames
  --mapM_ print sorted
