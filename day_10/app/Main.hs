module Main where

import Data.Vector.Unboxed (Vector, (!), fromList, findIndex)
import Data.List (maximumBy)
import Data.Ord (comparing)

data Matrix = Matrix (Vector Char) Int

-- parsing file
readFileAsLines :: FilePath -> IO [String]
readFileAsLines path = do
    content <- readFile path
    return $ lines content

linesToUnboxedVector :: [String] -> Vector Char
linesToUnboxedVector = fromList . concat

readFileToMatrix :: FilePath -> IO Matrix
readFileToMatrix path = do
    rawLines <- readFileAsLines path
    let width = length (head rawLines)
    return $ Matrix (linesToUnboxedVector rawLines) width

-- QoL, manage indices
transformIndex :: Int -> Int -> (Int, Int)
transformIndex width raw = (raw `div` width, raw `mod` width)

indexMatrix :: Matrix -> (Int, Int) -> Char
indexMatrix (Matrix vec width) (row, col) = vec ! (row * width + col)

getStartingIndex:: Matrix -> (Int, Int)
getStartingIndex (Matrix m w) = transformIndex w index
  where Just index = findIndex (\x -> x=='S') m -- unsafe unwrap

roadToLength:: Matrix -> (Int, Int) -> (Int, Int) -> (Int, [(Int,Int)]) -> (Int, [(Int,Int)])
roadToLength m (_, _) (i,j) (n, path)
  |  i < 0 || j < 0 = (0, [])
  |  indexMatrix m (i,j) == '.' = (-2, []) -- ended on a dot
  |  indexMatrix m (i,j) == 'S' = (n, (i,j):path)

roadToLength m (li, lj) (i, j) (n, path)

  -- comung from up
  | currSym == '|' && (li < i ) = roadToLength m (i,j) (i+1, j) newN -- i'm going down
  -- comung from down
  | currSym == '|' && (li > i ) = roadToLength m (i,j) (i-1, j) newN -- i'm going up

  -- coming from left
  | currSym == '-' && (lj < j ) = roadToLength m (i,j) (i, j+1) newN -- i'm going right
  -- coming from rigth
  | currSym == '-' && (lj > j ) = roadToLength m (i,j) (i, j-1) newN -- i'm going left

  --coming from up
  | currSym == 'L' && (li < i ) = roadToLength m (i,j) (i, j+1) newN -- i'm going rigth
  -- coming from rigth
  | currSym == 'L' && (lj > j ) = roadToLength m (i,j) (i-1, j) newN -- i'm going up

  -- coming from up
  | currSym == 'J' && (li < i ) = roadToLength m (i,j) (i, j-1) newN -- i'm going left
  -- coming drom left
  | currSym == 'J' && (lj < j ) = roadToLength m (i,j) (i-1, j) newN -- i'm going up

  -- coming from down
  | currSym == '7' && (li > i ) = roadToLength m (i,j) (i, j-1) newN -- i'm going left
  -- coming from left
  | currSym == '7' && (lj < j ) = roadToLength m (i,j) (i+1, j) newN -- i'm going down

  -- coming from rigth
  | currSym == 'F' && (lj > j ) = roadToLength m (i,j) (i+1, j) newN -- i'm going down
  -- coming from down
  | currSym == 'F' && (li > i ) = roadToLength m (i,j) (i, j+1) newN -- i'm going rigth
  | otherwise = (-1, []) -- ended on a disconnected pipe
    where
      currSym = indexMatrix m (i, j)
      newN = (n + 1, (i,j):path)

explorePaths:: Matrix -> (Int, Int) -> [(Int, [(Int, Int)])]
explorePaths matrix (i,j) =
  [n,e,s,w]
  where
    n = roadToLength matrix (i,j) (i-1,j) (1,[(i,j)]) -- go down
    e = roadToLength matrix (i,j) (i,j+1) (1,[(i,j)]) -- go down
    s = roadToLength matrix (i,j) (i+1,j) (1,[(i,j)]) -- go down
    w = roadToLength matrix (i,j) (i,j-1) (1,[(i,j)]) -- go down

shoeLace :: [(Int, Int)] -> Int
shoeLace [] = 0
shoeLace [_] = 0
shoeLace ((x1, y1) : (x2, y2) : xs) = (y1 + y2) * (x2 - x1) + shoeLace ((x2, y2) : xs)


getInnerArea :: [(Int, Int)] -> Int
getInnerArea points = (sl - lengthOutside + 3) `div` 2
  where
    sl = shoeLace points
    lengthOutside = length points

main :: IO ()
main = do
    matrix <- readFileToMatrix "input.txt"
    let
      (result, path) = maximumBy (comparing fst) (explorePaths matrix $ getStartingIndex matrix)
      res = (explorePaths matrix $ getStartingIndex matrix)
    print result -- part 1
    print $ getInnerArea path -- part 2
    mapM_ print res
