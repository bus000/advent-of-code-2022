{- --- Day 2: Rock Paper Scissors ---
 -
 - The Elves begin to set up camp on the beach. To decide whose tent gets to be
 - closest to the snack storage, a giant Rock Paper Scissors tournament is
 - already in progress.
 -
 - Rock Paper Scissors is a game between two players. Each game contains many
 - rounds; in each round, the players each simultaneously choose one of Rock,
 - Paper, or Scissors using a hand shape. Then, a winner for that round is
 - selected: Rock defeats Scissors, Scissors defeats Paper, and Paper defeats
 - Rock. If both players choose the same shape, the round instead ends in a
 - draw.
 -
 - Appreciative of your help yesterday, one Elf gives you an encrypted strategy
 - guide (your puzzle input) that they say will be sure to help you win. "The
 - first column is what your opponent is going to play: A for Rock, B for Paper,
 - and C for Scissors. The second column--" Suddenly, the Elf is called away to
 - help with someone's tent.
 -
 - The second column, you reason, must be what you should play in response: X
 - for Rock, Y for Paper, and Z for Scissors. Winning every time would be
 - suspicious, so the responses must have been carefully chosen.
 -
 - The winner of the whole tournament is the player with the highest score. Your
 - total score is the sum of your scores for each round. The score for a single
 - round is the score for the shape you selected (1 for Rock, 2 for Paper, and 3
 - for Scissors) plus the score for the outcome of the round (0 if you lost, 3
 - if the round was a draw, and 6 if you won).
 -
 - Since you can't be sure if the Elf is trying to help you or trick you, you
 - should calculate the score you would get if you were to follow the strategy
 - guide.
 -
 - For example, suppose you were given the following strategy guide:
 -
 -    A Y
 -    B X
 -    C Z
 -
 - This strategy guide predicts and recommends the following:
 -
 - * In the first round, your opponent will choose Rock (A), and you should
 -   choose Paper (Y). This ends in a win for you with a score of 8 (2 because
 -   you chose Paper + 6 because you won).
 - * In the second round, your opponent will choose Paper (B), and you should
 -   choose Rock (X). This ends in a loss for you with a score of 1 (1 + 0).
 - * The third round is a draw with both players choosing Scissors, giving you a
 -   score of 3 + 3 = 6.
 - * In this example, if you were to follow the strategy guide, you would get a
 -   total score of 15 (8 + 1 + 6).
 -
 - What would your total score be if everything goes exactly according to your
 - strategy guide?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P

data Object = Rock | Paper | Scissors deriving (Show, Eq, Ord)
data Result = Loss | Draw | Win deriving (Show, Eq, Ord)
type Strategy = (Object, Object)

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: [Strategy] -> IO ()
handleInput = print . sum . map (uncurry score)

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
parseStrategy = (,) <$> (parseObject <* P.space) <*> parseObject

parseObject :: P.Parsec T.Text () Object
parseObject = f <$> P.oneOf "ABCXYZ"
  where
    f 'A' = Rock
    f 'B' = Paper
    f 'C' = Scissors
    f 'X' = Rock
    f 'Y' = Paper
    f 'Z' = Scissors
    f _ = error "Not possible"
