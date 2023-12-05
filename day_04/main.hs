module Main where

import Data.Char (isDigit, digitToInt)
import Data.List (intersect)
import Text.ParserCombinators.Parsec
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Num.Backend.Native (count_words_bits)
import GHC.Weak (finalize)


data Game = Game Int [Int] [Int]
    deriving Show

type Input = [String]

test = "test.txt"

ex = "Card 1: 41 48 83 86 17 | 83 86  6 31 17 9 48 53"
ex2 = "41 48 83 86 17 | 83 86  6 31 17 9 48 53"
g1 = Game 1 [41,48,83,86,17] [83,86,6,31,17,9,48,53]

numberList :: Parser [Int]
numberList = many $ do
    number <- many1 digit
    spaces
    return $ read number

gameParser :: Parser Game
gameParser = do
    string "Card"
    spaces
    gameId <- many1 digit
    char ':'
    spaces
    leftNumbers <- numberList
    char '|'
    spaces
    rightNumbers <- numberList
    return $ Game (read gameId) leftNumbers rightNumbers

parseInput :: String -> Either ParseError [Game]
parseInput input = parse (sepBy gameParser newline) "" input

countWins:: Game -> Int
countWins (Game _ a b) =
  length $ intersect a b

calcPoints :: Int -> Int
calcPoints a
  | a <= 0 = 0
  | a == 1 = 1
  | otherwise = 2* calcPoints (a-1)

part1 :: IO ()
part1 = do
  text <- readFile "input.txt"
  let linesText = lines text
  let parsedGames = map (parse gameParser "") linesText
  let winCountsList = map (either (const 0) countWins) parsedGames  -- Using either to handle parse errors
  let winPoints = map (calcPoints . (either (const 0) countWins)) parsedGames  -- Assuming calcPoints exists
  let startingCards = replicate (length linesText) 1
  let finalCardNumber = applyAll winCountsList startingCards
  --print $ sum $ zipWith (*) winPoints finalCardNumber
  mapM_ print parsedGames
  print winCountsList
  --print $ sum finalCardNumber
  print startingCards
  print finalCardNumber
  print $ sum (zipWith (*) finalCardNumber winPoints)
  print $ sum winPoints

-- countValidCards wincountsList available visited -> visited++till available
countValidCards:: [Int] -> Int -> Int -> Int
countValidCards [] _ v = v
countValidCards (x:xs) n v
  | n <= 0 = v
  | otherwise = countValidCards xs ((n-1)+x) (v+1)


sumSkipped:: Int -> Int -> [Int] -> [Int]
sumSkipped _ _ [] = []
sumSkipped n a (b:bs)
  | a <= 0 = (b:bs)
  | otherwise = (b+n) : sumSkipped b (a-1) bs

applyAll:: [Int] -> [Int] -> [Int]
applyAll (x:xs) [] = []
applyAll [] y = y
applyAll (x:xs) (y:ys) = y : applyAll xs incrementedY
  where
    incrementedY = sumSkipped y x ys

p1 =32609
p2 = 14624680
