import Data.Char (isDigit) -- isSymbol) --, digitToInt)
import Data.List (isPrefixOf, nub)--, group)
--import Distribution.Simple.Utils (xargs)
-- nub for taking uniques in recyursion I took the first digit position twice

sensitivePositions:: [Int] -> [Int]
sensitivePositions x = y
  where
    y = nub $ filter (\y -> y >= 0) [(head x)-1 .. ((last x)+1)]

getIntsAndPositions :: String -> [(Int, [Int])]
getIntsAndPositions str = map (\(num, pos) -> (read num, pos)) parsedNumsAndPos
  where
    parsedNumsAndPos = map (\x -> (filter isDigit x, sensitivePositions $ findAllIndices (filter isDigit x) str)) intStrs
    intStrs = filter (any isDigit) $ groupByAdjacentDigits str

groupByAdjacentDigits :: String -> [String]
groupByAdjacentDigits [] = []
groupByAdjacentDigits (x:xs)
  | isDigit x = let (digits, rest) = span isDigit (x:xs)
                in digits : groupByAdjacentDigits rest
  | otherwise = groupByAdjacentDigits xs

findAllIndices :: Eq a => [a] -> [a] -> [Int]
findAllIndices xs = go 0
  where
    go _ [] = []
    go n ys
      | xs `isPrefixOf` ys = n : map (+ n) [0 .. length xs - 1] ++ go (n + 1) (tail ys)
      | otherwise = go (n + 1) (tail ys)

getHotPositions::String -> Int -> [Int]
getHotPositions [] _ = []
getHotPositions (x:xs) n
  | isaHot x = n: getHotPositions xs (n+1)
  | otherwise = getHotPositions xs (n+1)


getGearPositions::String -> Int -> [Int]
getGearPositions [] _ = []
getGearPositions (x:xs) n
  | isaGear x = n: getGearPositions xs (n+1)
  | otherwise = getGearPositions xs (n+1)


part1 :: IO ()
part1 = do
  contents <- readFile "input.txt"
  let linesOfFile = lines contents
      hotPositions = map (\line -> getHotPositions line 0) linesOfFile
      intsAndPositions = map getIntsAndPositions linesOfFile
      hp = map fuse $ zip (map fuse $ zip hotPositions ((tail hotPositions)++ [[]])) ([[]] ++ hotPositions)
      m = zipWith checkOverlap intsAndPositions hp
      result = sum $ map sum m -- 304
      gearPositions = map (\line -> getGearPositions line 0) linesOfFile  -- Apply getHotPositions to each line
      gp_extended = map fuse $ zip
        (map fuse $ zip gearPositions ((tail gearPositions)++ [[]]))
        ([[]] ++ gearPositions)
  --print $ length m
  --mapM_ print m
  --print $ sum $ map sum m
  --mapM_ print $ map (filter (\x -> x < 305)) m
  print intsAndPositions
  print $ sum $ map sum m
  print $ zip gp_extended [0..]

fuse :: ([Int],[Int]) -> [Int]
fuse (a,b) = c where
  c = nub $ a++b

checkOverlap :: [(Int, [Int])] -> [Int] -> [Int]
checkOverlap tuples list =
  [num | (num, positions) <- tuples, any (`elem` list) positions]

isaHot:: Char -> Bool
isaHot c = c /= '.' && (not $ isDigit c)

isaGear:: Char -> Bool
isaGear c = c == '*'
