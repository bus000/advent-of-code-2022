{- --- Part Two ---
 -
 - The Elf finishes helping with the tent and sneaks back over to you. "Anyway,
 - the second column says how the round needs to end: X means you need to lose,
 - Y means you need to end the round in a draw, and Z means you need to win.
 - Good luck!"
 -
 - The total score is still calculated in the same way, but now you need to
 - figure out what shape to choose so the round ends as indicated. The example
 - above now goes like this:
 -
 - * In the first round, your opponent will choose Rock (A), and you need the
 -   round to end in a draw (Y), so you also choose Rock. This gives you a score
 -   of 1 + 3 = 4.
 - * In the second round, your opponent will choose Paper (B), and you choose
 -   Rock so you lose (X) with a score of 1 + 0 = 1.
 - * In the third round, you will defeat your opponent's Scissors with Rock for
 -   a score of 1 + 6 = 7.
 -
 - Now that you're correctly decrypting the ultra top secret strategy guide, you
 - would get a total score of 12.
 -
 - Following the Elf's instructions for the second column, what would your total
 - score be if everything goes exactly according to your strategy guide?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P

data Object = Rock | Paper | Scissors deriving (Show, Eq, Ord)
data Result = Loss | Draw | Win deriving (Show, Eq, Ord)
type Strategy = (Object, Result)
type Game = (Object, Object)

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: [Strategy] -> IO ()
handleInput = print . sum . map (uncurry score . chooseObject)

chooseObject :: Strategy -> Game
chooseObject (them, Draw) = (them, them)
chooseObject (Rock, Win) = (Rock, Paper)
chooseObject (Rock, Loss) = (Rock, Scissors)
chooseObject (Paper, Win) = (Paper, Scissors)
chooseObject (Paper, Loss) = (Paper, Rock)
chooseObject (Scissors, Win) = (Scissors, Rock)
chooseObject (Scissors, Loss) = (Scissors, Paper)

score :: Object -> Object -> Word
score them us = shapeScore + winScore
  where
    shapeScore = case us of
        Rock -> 1
        Paper -> 2
        Scissors -> 3

    winScore = case result them us of
        Loss -> 0
        Draw -> 3
        Win -> 6

result :: Object -> Object -> Result
result Rock Scissors = Loss
result Paper Rock = Loss
result Scissors Paper = Loss
result Rock Rock = Draw
result Paper Paper = Draw
result Scissors Scissors = Draw
result Rock Paper = Win
result Paper Scissors = Win
result Scissors Rock = Win

parseInput :: T.Text -> Either P.ParseError [Strategy]
parseInput = P.parse (parseStrategies <* P.eof) ""

parseStrategies :: P.Parsec T.Text () [Strategy]
parseStrategies = parseStrategy `P.endBy` P.newline

parseStrategy :: P.Parsec T.Text () Strategy
parseStrategy = (,) <$> (parseObject <* P.space) <*> parseResult

parseObject :: P.Parsec T.Text () Object
parseObject = f <$> P.oneOf "ABC"
  where
    f 'A' = Rock
    f 'B' = Paper
    f 'C' = Scissors
    f _ = error "Not possible"

parseResult :: P.Parsec T.Text () Result
parseResult = f <$> P.oneOf "XYZ"
  where
    f 'X' = Loss
    f 'Y' = Draw
    f 'Z' = Win
    f _ = error "Not possible"
