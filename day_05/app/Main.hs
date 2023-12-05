module Main where

import qualified Data.Map as Map

import Data.Maybe (catMaybes)


import Data.List.Split (splitOn)

import Control.Applicative (some)
import Control.Arrow ((&&&))

import Data.Map.Strict qualified as M
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative (RE, sym, string, psym, (=~))
import Text.Regex.Applicative.Common (decimal)

data MapItem = MapItem {dest :: Int, src :: Int, len :: Int} deriving (Show)

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


parseMapItem :: String -> MapItem
parseMapItem line = case map read $ words line of
    [d, s, l] -> MapItem d s l
    _ -> error "Invalid input"

parseMap :: String -> [MapItem]
parseMap = map parseMapItem . tail . lines

parseMaps :: String -> ([Int], [[MapItem]])
parseMaps input = case splitOn "\n\n" input of
    (x : ls) -> (map read $ tail $ words x, map parseMap ls)
    _ -> undefined

mapRange :: Int -> [MapItem] -> Int
mapRange x [] = x
mapRange x (MapItem d s l : xs)
    | s <= x && x < s + l = d + x - s
    | otherwise = mapRange x xs

part1 :: String -> String
part1 input = show $ minimum $ map (\x -> foldl mapRange x maps) seeds
  where
    (seeds, maps) = parseMaps input

mapRange' :: (Int, Int) -> [MapItem] -> [(Int, Int)]
mapRange' x [] = [x]
mapRange' (rs, rl) (MapItem d s l : ms)
    | rs <= s + l && s < rs + rl = pre ++ curr ++ post
    | otherwise = mapRange' (rs, rl) ms
  where
    pre = if rs < s then mapRange' (rs, s - rs) ms else []
    curr = [(d + max 0 (rs - s), min rl (l - max 0 (rs - s)))]
    post = if s + l < rs + rl then mapRange' (s + l, rs + rl - s - l) ms else []

pairUp :: [a] -> [(a, a)]
pairUp [] = []
pairUp (x : y : rest) = (x, y) : pairUp rest
pairUp _ = error "Input list should have an even number of elements."

-- not working
part2 :: String -> String
part2 input = show $ fst $ minimum $ foldl mapRange'' (pairUp seeds) maps
  where
    (seeds, maps) = parseMaps input
    mapRange'' :: [(Int, Int)] -> [MapItem] -> [(Int, Int)]
    mapRange'' xs ms = concatMap (`mapRange'` ms) xs

main2 :: IO ()
main2 = do
  y  <- readFile "input.txt"
  print $ part1 y
  print $ part2 y
