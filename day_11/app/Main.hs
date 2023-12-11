module Main where
import Data.List(nub, sort, (\\), intercalate)

data Universe = Universe [Int] [Int] Int Int deriving(Show)

getPositions:: Int -> [Char] -> [Int]
getPositions _ [] = []
getPositions i (c:cs)
  | c == '#' = i: getPositions (i+1) cs
  | otherwise = getPositions (i+1) cs

relaxPositions:: ([Int], Int) -> [(Int, Int)]
relaxPositions ([], _ ) = []
relaxPositions ([a], i ) = [(a,i)]
relaxPositions (a:as, i) = (a,i):relaxPositions(as,i)

expandUniverse:: Universe -> Universe
expandUniverse (Universe xs ys maxX maxY) = Universe xs2 ys2 maxX2 maxY2
  where
    empty_rows = [0..maxX] \\ sort ( nub xs)
    empty_columns = [0..maxY] \\ sort ( nub ys)
    (xs2, maxX2) = expand xs empty_rows maxX
    (ys2, maxY2) = expand ys empty_columns maxY

expand:: [Int] -> [Int] -> Int -> ([Int], Int)
expand l [] dim = (l, dim)
expand l (n:ns) dim = expand l2 ns dim2
  where
    l2 = [if x > n
          then x + 1
          else x
         | x <- l]
    dim2 = dim + 1

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

solveDistances :: Universe -> Int
solveDistances (Universe xs ys _ _) =
  sum [manhattanDistance p1 p2 |
       (i, p1) <- zip [0..] points,
       p2 <- drop (i + 1) points]
  where
    points = zip xs ys

distanceMatrix :: Universe -> [[Int]]
distanceMatrix (Universe xs ys _ _) = [[manhattanDistance p1 p2 | p2 <- points] | p1 <- points]
  where
    points = zip xs ys

-- Function to print the grid
printUniverse :: Universe -> IO ()
printUniverse (Universe xs ys maxX maxY) =
  putStrLn $ unlines [ [if (x, y) `elem` points
                        then '#'
                        else '.'
                       | x <- [0..maxX-1]]
                     | y <- [0..maxY-1]]
  where points = zip xs ys

printGrid :: Universe -> IO ()
printGrid (Universe xs ys maxX maxY) =
  mapM_ putStrLn [ [if (x, y) `elem` points then '#' else '.' | x <- [0..maxX-1]] | y <- [0..maxY-1]]
  where
    points = zip xs ys


main :: IO ()
main = do
  rawText <- readFile "test.txt"
  let linesText = lines rawText
  let maxX = length linesText
  let maxY = length $ head linesText
  let positions = map (getPositions 0) $ linesText
  let points = concatMap relaxPositions $ zip positions [0..]
  let (xs,ys) = unzip points
  let u1 = Universe xs ys maxX maxY
  let u2 = expandUniverse $ Universe xs ys maxX maxY
  print u1
  print u2
  printGrid u1
  printGrid u2
  print $ solveDistances u2
  mapM_ print $ distanceMatrix u2
