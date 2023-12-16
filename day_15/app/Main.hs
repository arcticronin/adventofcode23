module Main where
import Data.List.Split (splitOn)
import Data.Char (ord)
-- import Data.List
import qualified Data.IntMap as M

-- part 2 data and types

type HashMap = M.IntMap [Lente]
data Operation = Add Lente | Remove Lente deriving (Show)
data Lente = Lente String Int deriving (Show)


parse :: String -> String -> Operation
parse item ('=' : is) = Add $ Lente (reverse item) (read is)
parse item ('-' : _) = Remove $ Lente (reverse item) 0
parse item (x : is) = parse (x : item) is
parse _ []  = Remove $ Lente [] 0 -- error case


main :: IO ()
main = do
  rawFile <- readFile "input.txt"
  let text = rawFile
      input = splitOn ","  text
      -- part1
      x2 = map (map ord)  input
      -- part2
      y2 = map (parse "" ) input
  print "Part 1"
  print $ sum (toHash <$> x2)
  print "Part 2"
  print $ (sum . map (uncurry focusingPowers) . M.toList . applyOperations) y2

toHash :: [Int] -> Int
toHash lst = foldl (\acc x -> (acc + x) * 17 `mod` 256) 0 lst


instance Eq Lente where
  (==) (Lente label1 _) (Lente label2 _) = label1 == label2

initHashMap :: HashMap
initHashMap = M.fromList $ zip [0..] (replicate 256 [])

insertLens :: Lente -> HashMap -> HashMap
insertLens l m = M.insert hash (insertIntoBucket l lensBucket) m
  where
    hash = lensHash l
    lensBucket = m M.! hash

insertIntoBucket :: Lente -> [Lente] -> [Lente]
insertIntoBucket l ls = before ++ newAfter
  where
    (before, after) = break (== l) ls
    newAfter = if null after then [l] else l:tail after

removeLens :: Lente -> HashMap -> HashMap
removeLens l m = M.insert hash (removeFromBucket l lensBucket) m
  where
    hash = lensHash l
    lensBucket = m M.! hash

removeFromBucket :: Lente -> [Lente] -> [Lente]
removeFromBucket l ls = before ++ newAfter
  where
    (before, after) = break (== l) ls
    newAfter = if null after then [] else tail after

focusingPower :: Int -> Int -> Lente -> Int
focusingPower boxNr bucketNr l = (boxNr + 1) * bucketNr * lensFocalLength l

focusingPowers :: Int -> [Lente] -> Int
focusingPowers boxNr = sum . zipWith (focusingPower boxNr) [1..]

applyOperations :: [Operation] -> HashMap
applyOperations = foldl (flip applyOperation) initHashMap

applyOperation :: Operation -> HashMap -> HashMap
applyOperation (Add lente) = insertLens lente
applyOperation (Remove lente) = removeLens lente


-- to quick go into fields
lensLabel :: Lente -> String
lensLabel (Lente label _) = label

lensFocalLength :: Lente -> Int
lensFocalLength (Lente _ f) = f

lensHash :: Lente -> Int
lensHash = toHash . map ord . lensLabel
