{- --- Part Two ---
 -
 - By the time you calculate the answer to the Elves' question, they've already
 - realized that the Elf carrying the most Calories of food might eventually run
 - out of snacks.
 -
 - To avoid this unacceptable situation, the Elves would instead like to know
 - the total Calories carried by the top three Elves carrying the most Calories.
 - That way, even if one of those Elves runs out of snacks, they still have two
 - backups.
 -
 - In the example above, the top three Elves are the fourth Elf (with 24000
 - Calories), then the third Elf (with 11000 Calories), then the fifth Elf (with
 - 10000 Calories). The sum of the Calories carried by these three elves is
 - 45000.
 -
 - Find the top three Elves carrying the most Calories. How many Calories are
 - those Elves carrying in total?
 -}
module Main where

import AdventOfCode
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Text as T
import qualified Data.List as L

main :: IO ()
main = defaultMain parseInput handleInput

type Calories = Word

handleInput :: [[Calories]] -> IO ()
handleInput = print . sum . take 3 . sortDesc . map sum
  where
    sortDesc = L.sortBy (flip compare)

parseInput :: T.Text -> Either P.ParseError [[Calories]]
parseInput = P.parse (parseElfs <* P.eof) ""

parseElfs :: P.Parsec T.Text () [[Calories]]
parseElfs = parseElf `P.sepBy` P.newline

parseElf :: P.Parsec T.Text () [Calories]
parseElf = P.int `P.endBy` P.newline
