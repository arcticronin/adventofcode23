module Main where

import qualified Data.Map as Map

import Data.Maybe (catMaybes)
import Control.Applicative (some)
import Text.Regex.Applicative (RE, sym, string, (=~))
import Text.Regex.Applicative.Common (decimal)

-- Data here
data Input  = Input Seeds [Categ] deriving (Show)
data Seeds = Seeds [Int] deriving (Show)
data Categ = Categ [(Int, Int, Int)] deriving (Show)  -- Updated to hold a list of triplets

-- types here
type Parser a = RE Char a

spaces :: Parser ()
spaces = () <$ some (sym ' ')

input :: [String] -> Input
input  [] = undefined
input (x:xs) = Input s (parseCategories xs)
  where
    s = maybe (Seeds [-1]) id $ x =~ seeds

parseCategories :: [String] -> [Categ]
parseCategories [] = []
parseCategories (l:ls)
  | isCategoryTitle l = parseCategory
    (takeWhile (not . isCategoryTitle) ls) :
    parseCategories (dropWhile (not . isCategoryTitle) ls)
  | otherwise = parseCategories ls

isCategoryTitle :: String -> Bool
isCategoryTitle line = not (null line) && last line == ':'

parseCategory :: [String] -> Categ
parseCategory = Categ . catMaybes . map (=~ tripletta)

seeds :: Parser Seeds
seeds = Seeds <$> (string "seeds:" *> some number)
  where
    number = spaces *> decimal

tripletta :: Parser (Int, Int, Int)
tripletta = (,,) <$> decimal <*> (spaces *> decimal) <*> (spaces *> decimal)

tripletToMap :: (Int, Int, Int) -> Map.Map Int Int
tripletToMap (j, i, r) =
  Map.fromList $ zip [i..i+r] [j..j+r]

createUnifiedMap :: Categ -> Map.Map Int Int
createUnifiedMap (Categ triplets) = foldl Map.union Map.empty maps
  where
    maps = map tripletToMap triplets

applyMap :: Map.Map Int Int -> Int -> Int
applyMap catmap a =
    case Map.lookup a catmap of
      Just b -> b
      Nothing -> a

main :: IO ()
main = do
  linesTextRaw <- readFile "input.txt"
  let linesTextList = lines linesTextRaw
  let Input (Seeds sds) cts = input linesTextList
  let catmaps = map createUnifiedMap cts
  let fncs = map applyMap catmaps
  let results = map (\seed -> foldl (\acc f -> f acc) seed fncs) sds

  print sds
  --mapM_ print cts  -- Print each category
  print "risultati"
  --print results
  print $ minimum results
