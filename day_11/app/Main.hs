module Main where
import Data.List(nub, sort, (\\))

data Universe = Universe [(Int,Int)] Int Int deriving(Show)

getPositions:: Int -> [Char] -> [Int]
getPositions _ [] = []
getPositions i (c:cs)
  | c == '#' = i: getPositions (i+1) cs
  | otherwise = getPositions (i+1) cs

relaxPositions:: ([Int], Int) -> [(Int, Int)]
relaxPositions ([], _ ) = []
relaxPositions ([a], i ) = [(i,a)]
relaxPositions (a:as, i) = (i,a):relaxPositions(as,i)

expandUniverse:: Universe -> Universe
expandUniverse (Universe points maxRow maxColumn) = Universe points2 maxC2 maxC2
  where
    (rs, cs) = unzip points
    empty_rows = reverse (sort [0..maxRow-1] \\ nub rs)
    empty_columns = reverse (sort [0..maxColumn-1] \\ nub cs)
    (rs2, maxR2) = expand rs empty_rows maxRow
    (cs2, maxC2) = expand cs empty_columns maxColumn
    points2 = zip rs2 cs2

expand :: [Int] -> [Int] -> Int -> ([Int], Int)
expand coord [] size = (coord, size)
expand coord (i:is) size =  expand newCoord is newSize
  where
    --newCoord = [if x > i then x + 1 else x | x <- coord] -- Part 1
    newCoord = [if x > i then (x+999999) else x | x <- coord] -- Part 2
    newSize = size + 1

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

solveDistances :: Universe -> Int
solveDistances (Universe points _ _) =
  sum [manhattanDistance p1 p2 |
       (i, p1) <- zip [0..] points,
       p2 <- drop (i + 1) points]

main :: IO ()
main = do
  rawText <- readFile "input.txt"
  let linesText = lines rawText
  let maxRows = length linesText
  let maxColumns = length $ head linesText
  let positions = map (getPositions 0) $ linesText
  let points = concatMap relaxPositions $ zip positions [0..]
  let u1 = Universe points maxRows maxColumns
  let u2 = expandUniverse u1
  print $ solveDistances u2
