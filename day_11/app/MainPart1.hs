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
    newCoord = [if x > i then x + 1 else x | x <- coord]
    newSize = size + 1

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

solveDistances :: Universe -> Int
solveDistances (Universe points _ _) =
  sum [manhattanDistance p1 p2 |
       (i, p1) <- zip [0..] points,
       p2 <- drop (i + 1) points]

distanceMatrix :: Universe -> [[Int]]
distanceMatrix (Universe points _ _) = [[manhattanDistance p1 p2 | p2 <- points] | p1 <- points]

printGrid :: Universe -> IO ()
printGrid (Universe points maxRow maxColumn) =
  mapM_ putStrLn [ [if (r, c) `elem` points then '#' else '.' | c <- [0..maxColumn-1]] | r <- [0..maxRow-1]]

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
  print "positions"
  print positions
  print "u1"
  print u1
  printGrid u1
  print "u2"
  print u2
  printGrid u2
  print $ solveDistances u2
  let (rs, cs) = unzip points
  let empty_rows = [0..maxRows-1] \\ nub rs
  let empty_columns = [0..maxColumns-1] \\ nub cs

  -- Now you can print empty_rows and empty_columns for debugging
  print "Empty Rows:"
  print empty_rows
  print "Empty Columns:"

  print empty_columns
  let (rs2, _) = expand rs empty_rows maxRows
  let (cs2, _) = expand cs empty_columns maxColumns
  print "Original Rows:Columns"
  print points
  print "Expanded Rows:Columns"
  print $ zip rs2 cs2
