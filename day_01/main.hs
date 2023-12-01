import Data.Char (isDigit, digitToInt)

-- reimplementation of is prefix of, i am trying to use less modules possible
isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys)
  | x == y = isPrefixOf xs ys
  | otherwise = False

-- note, int this example I dropped one letter less of the expected, because
-- there can be superpositions: eightwo -> 82
-- the example didint cover this case I had to struggle a little bit
preparse :: String -> String
preparse [] = []
preparse str@(x:xs)
  | isPrefixOf "one" str = '1' : preparse (drop 2 str)
  | isPrefixOf "two" str = '2' : preparse (drop 2 str)
  | isPrefixOf "three" str = '3' : preparse (drop 4 str)
  | isPrefixOf "four" str = '4' : preparse (drop 3 str)
  | isPrefixOf "five" str = '5' : preparse (drop 3 str)
  | isPrefixOf "six" str = '6' : preparse (drop 2 str)
  | isPrefixOf "seven" str = '7' : preparse (drop 4 str)
  | isPrefixOf "eight" str = '8' : preparse (drop 4 str)
  | isPrefixOf "nine" str = '9' : preparse (drop 3 str)
  | otherwise = x : (preparse xs)

-- returns first and last only, if it is only one digit, double it, as example
parseLine :: String -> String
parseLine s
  | length (getfirst s) == 1 = concat [(getfirst s),(getfirst s)]
  | otherwise = getfirst s


getfirst :: String -> String
getfirst [] = ""
getfirst (x:xs)
  | isDigit x = x : getlast (reverse xs)
  | otherwise = getfirst xs

getlast :: String -> String
getlast [] = ""
getlast (x:xs)
  | isDigit x = [x]
  | otherwise = getlast xs


calcolaRiga :: String -> Int
-- i want to fold left, so i can increase the power 10^i every number i will find in the natural direction
-- i wrote this before realising we actually have to cover just 2 digits case, but it works for 2 or more
calcolaRiga str = foldl (\i x -> (i * 10 + x)) 0 (map digitToInt ( parseLine (preparse str)))

applyOnFile :: FilePath -> IO(Int)
applyOnFile fpath = do
  text <- readFile fpath
  return (sum (map calcolaRiga (lines text)))

--main example
maine :: IO ()
maine = do
  calc <- applyOnFile "example2.txt"
  print calc

--main on challenge
main :: IO ()
main = do
  calc <- applyOnFile "challenge.txt"
  print calc
