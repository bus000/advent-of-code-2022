{- It seems like there is still quite a bit of duplicate work planned. Instead,
 - the Elves would like to know the number of pairs that overlap at all.
 -
 - In the above example, the first two pairs (2-4,6-8 and 2-3,4-5) don't
 - overlap, while the remaining four pairs (5-7,7-9, 2-8,3-7, 6-6,4-6, and
 - 2-6,4-8) do overlap:
 -
 - * 5-7,7-9 overlaps in a single section, 7.
 - * 2-8,3-7 overlaps all of the sections 3 through 7.
 - * 6-6,4-6 overlaps in a single section, 6.
 - * 2-6,4-8 overlaps in sections 4, 5, and 6.
 -
 - So, in this example, the number of overlapping assignment pairs is 4.
 -
 - In how many assignment pairs do the ranges overlap?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = defaultMain parseInput handleInput

data Assignment = Assignment
    { _worker1 :: !Worker
    , _worker2 :: !Worker
    } deriving (Show, Eq, Ord)

data Worker = Worker
    { _start :: !Word
    , _end   :: !Word
    } deriving (Show, Eq, Ord)

handleInput :: [Assignment] -> IO ()
handleInput = print . length . filter wasteful

wasteful :: Assignment -> Bool
wasteful (Assignment (Worker s1 e1) (Worker s2 e2)) =
    (s1 >= s2 && s1 <= e2) ||
    (e1 <= e2 && e1 >= s2) ||
    (s2 >= s1 && s2 <= e1) ||
    (e2 <= e1 && e2 >= s1)

newWorker :: Word -> Word -> Worker
newWorker n1 n2
    | n1 < n2 = Worker n1 n2
    | otherwise = Worker n2 n1

parseInput :: T.Text -> Either P.ParseError [Assignment]
parseInput = P.parse (parseAssignments <* P.eof) ""

parseAssignments :: P.Parsec T.Text () [Assignment]
parseAssignments = parseAssignment `P.endBy` P.newline

parseAssignment :: P.Parsec T.Text () Assignment
parseAssignment = Assignment <$> parseWorker <* P.char ',' <*> parseWorker

parseWorker :: P.Parsec T.Text () Worker
parseWorker = newWorker <$> P.int <* P.char '-' <*> P.int
