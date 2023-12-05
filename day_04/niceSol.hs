module Main where

import Control.Arrow (second, (&&&))
import Data.List (intersect)
import Data.Tuple.Extra (both)

main :: IO ()
main = interact $ (++ "\n") . show . (part1 &&& part2) . map parse . lines

part1 :: [([Int], [Int])] -> Int
part1 = sum . map ((\n -> if n >= 1 then 2 ^ (n - 1) else 0) . countMatches)

part2 :: [([Int], [Int])] -> Int
part2 = sum . foldr (\c l -> 1 + sum (take (countMatches c) l) : l) []

countMatches :: ([Int], [Int]) -> Int
countMatches = length . uncurry intersect

parse :: String -> ([Int], [Int])
parse = both (map read) . second tail . span (/= "|") . drop 2 . words