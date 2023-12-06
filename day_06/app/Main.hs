module Main where

import Data.Maybe (mapMaybe)
import Text.Regex.Applicative (RE, sym, psym, (=~), many)
import Text.Regex.Applicative.Common (decimal)

import Data.Fixed (HasResolution(resolution))

-- Types
type Heading = String
type NumList = [Int]

-- Data
data Elem = Elem Heading NumList deriving (Show)

-- Parsing
elemParser :: RE Char Elem
elemParser = fmap Elem heading <* sym ':' <*> numList

heading :: RE Char Heading
heading = many $ psym (/= ':')

numList :: RE Char NumList
numList = many $ (spaces  *> decimal)

spaces :: RE Char String
spaces = many $ (sym ' ')

simulateRace :: (Int, Int) -> Int
simulateRace (t, d) = lastGood - firstGood
  where
    -- finds the first number that makes me win
    simulateAux :: (Int, Int) -> Int -> Int
    simulateAux (t, d) p
      | p >= t = 0
      | p * (t - p) > d = p
      | otherwise = simulateAux (t, d) (p + 1)
    -- it is supposed to go from the first "good number" to find the last
    simulateAux2 :: (Int, Int) -> Int -> Int
    simulateAux2 (t, d) p
      | p >= t = 0
      | p * (t - p) <= d = p
      | otherwise = simulateAux2 (t, d) (p + 1)

    firstGood = simulateAux (t, d) 0
    lastGood = simulateAux2 (t, d) firstGood

concatenateAndConvert :: [Int] -> Int
concatenateAndConvert nums = read (concatMap show nums) :: Int

main :: IO ()
main = do
  rawLines <- readFile "input.txt"
  let elems = mapMaybe (=~ elemParser) $ lines rawLines
  let Elem _ times = head elems
  let Elem _ distances = head $ tail elems
  let x = zip times distances
  print "Part 1:"
  print "(time, distance)"
  print x
  print $ map simulateRace x
  print $ product $ map simulateRace x
  print ""
  print "Part 2"
  let p2t = concatenateAndConvert times
  let p2d = concatenateAndConvert distances
  print "(time, distance)"
  print (p2t, p2d)
  print $ simulateRace (p2t, p2d)


-- recursive solution stack overflow on part 2
simulateRaceRec:: (Int, Int) -> Int -> Int
simulateRaceRec (t, d) p
  | p >= t = 0
  | p * (t - p) > d =  1 + simulateRaceRec (t, d) (p+1)
  | p * (t - p) <= d = simulateRaceRec (t, d) (p+1)

-- folding solution : still stack overflow
simulateRaceFold :: (Int, Int) -> Int
simulateRaceFold (t, d) = foldr (\p acc -> if p * (t - p) > d then acc + 1 else acc) 0 [0..t-1]

