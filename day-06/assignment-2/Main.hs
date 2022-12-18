{- --- Part Two ---
 -
 - Your device's communication system is correctly detecting packets, but still
 - isn't working. It looks like it also needs to look for messages.
 -
 - A start-of-message marker is just like a start-of-packet marker, except it
 - consists of 14 distinct characters rather than 4.
 -
 - Here are the first positions of start-of-message markers for all of the above
 - examples:
 -
 - * mjqjpqmgbljsphdztnvjfqwrcgsmlb: first marker after character 19
 - * bvwbjplbgvbhsrlpgdmjqwftvncz: first marker after character 23
 - * nppdvjthqldpwncqszvftbrmjlhg: first marker after character 23
 - * nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg: first marker after character 29
 - * zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw: first marker after character 26
 -
 - How many characters need to be processed before the first start-of-message
 - marker is detected?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.Set as Set

main :: IO ()
main = defaultMain parseInput handleInput

data Window a = Window a [a]

handleInput :: String -> IO ()
handleInput = print . (+14) . length  . takeMessage

takeMessage :: Ord a => [a] -> [a]
takeMessage = unSlideWindow . takeWhile windowHasDuplicates . slideWindow 14

slideWindow :: Int -> [a] -> [Window a]
slideWindow n (x:rest) = Window x (take (n - 1) rest) : slideWindow n rest
slideWindow _ _ = []

unSlideWindow :: [Window a] -> [a]
unSlideWindow [] = []
unSlideWindow (Window x _:rest) = x : unSlideWindow rest

windowHasDuplicates :: Ord a => Window a -> Bool
windowHasDuplicates (Window x xs) = hasDuplicates (x:xs)

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates = go Set.empty
  where
    go _ [] = False
    go seen (x:xs)
        | x `Set.member` seen = True
        | otherwise = go (Set.insert x seen) xs

parseInput :: T.Text -> Either P.ParseError String
parseInput = P.parse (P.many P.anyChar <* P.eof) ""
