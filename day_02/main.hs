import Data.Char (isDigit, digitToInt)
import Text.ParserCombinators.Parsec
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

example = "Game 12: 19 green, 3 blue, 10 red; 8 red, 2 blue, 19 green; 3 blue, 6 red, 2 green; 8 red, 5 blue; 1 blue, 15 green; 8 green, 7 red"
impossible = "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
data SinglePlay = SinglePlay { green :: Int, blue :: Int, red :: Int }
  deriving Show

data Game = Game Int [SinglePlay]
    deriving Show

parseSinglePlay :: Parser SinglePlay
parseSinglePlay = do
    counts <- many $ try (do
                          optional space
                          parseColorCount) <* optional (char ',')
    let colorMap = Map.fromListWith (+) counts
    let getCol c = Map.findWithDefault 0 c colorMap
    return ( SinglePlay (getCol "green") (getCol "blue") (getCol "red"))

parseColorCount :: Parser (String, Int)
parseColorCount = do
    count <- many digit
    optional space
    color <- choice [string "green", string "blue", string "red"]
    return (color, read count)

gameParser :: Parser Game
gameParser = do
    string "Game"
    spaces
    gameId <- many digit
    char ':'
    spaces
    plays <- sepBy parseSinglePlay (try (string "; ") <|> string ";")
    return ( Game (read gameId) plays)

-- quick debug games
possibleGame = Game 12 [SinglePlay {green = 19, blue = 3, red = 10},SinglePlay {green = 19, blue = 2, red = 8},SinglePlay {green = 2, blue = 3, red = 6},SinglePlay {green = 0, blue = 5, red = 8},SinglePlay {green = 15, blue = 1, red = 0},SinglePlay {green = 8, blue = 0, red = 7}]
impossibleGame = Game 3 [SinglePlay {green = 8, blue = 6, red = 20},SinglePlay {green = 13, blue = 5, red = 4},
               SinglePlay {green = 5, blue = 0, red = 1}]


countMaxColorsInGame :: Game -> (Int, Int, Int)
countMaxColorsInGame (Game _ plays) = foldl sumColors (0, 0, 0) plays
  where
    sumColors (gTotal, bTotal, rTotal) (SinglePlay g b r) =
      (max gTotal g, max bTotal b, max rTotal r)

limit :: (Int, Int, Int)
limit = (13, 14, 12)

compareTs :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
compareTs (a1, b1, c1) (a2, b2, c2) = (a1 >= a2)&&(b1 >= b2)&&(c1>=c2)

powerTuple:: (Int, Int, Int) -> Int
powerTuple (a, b, c) = a*b*c

part1 = do
  text <- readFile "input.txt"
  let linesText = lines text
  let parsedGames = map (parse gameParser "") linesText
  let maxColors = map (either (\_ -> (0, 0, 0)) countMaxColorsInGame) parsedGames
  let result = map (compareTs limit) maxColors
  print $ sum $ map snd $ filter fst $ zip result [1..]

part2 = do
  text <- readFile "input.txt"
  let linesText = lines text
  let parsedGames = map (parse gameParser "") linesText
  let maxColors = map (either (\_ -> (0, 0, 0)) countMaxColorsInGame) parsedGames
  print $ sum $ map powerTuple maxColors

