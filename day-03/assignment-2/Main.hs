{- As you finish identifying the misplaced items, the Elves come to you with
 - another issue.
 -
 - For safety, the Elves are divided into groups of three. Every Elf carries a
 - badge that identifies their group. For efficiency, within each group of three
 - Elves, the badge is the only item type carried by all three Elves. That is,
 - if a group's badge is item type B, then all three Elves will have item type B
 - somewhere in their rucksack, and at most two of the Elves will be carrying
 - any other item type.
 -
 - The problem is that someone forgot to put this year's updated authenticity
 - sticker on the badges. All of the badges need to be pulled out of the
 - rucksacks so the new authenticity stickers can be attached.
 -
 - Additionally, nobody wrote down which item type corresponds to each group's
 - badges. The only way to tell which item type is the right one is by finding
 - the one item type that is common between all three Elves in each group.
 -
 - Every set of three lines in your list corresponds to a single group, but each
 - group can have a different badge item type. So, in the above example, the
 - first group's rucksacks are the first three lines:
 -
 -    vJrwpWtwJgWrhcsFMMfFFhFp
 -    jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
 -    PmmdzqPrVvPwwTWBwg
 -
 - And the second group's rucksacks are the next three lines:
 -
 -    wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
 -    ttgJtRGJQctTZtZT
 -    CrZsJsPPZsGzwwsLwLmpwMDw
 -
 - In the first group, the only item type that appears in all three rucksacks is
 - lowercase r; this must be their badges. In the second group, their badge item
 - type must be Z.
 -
 - Priorities for these items must still be found to organize the sticker
 - attachment efforts: here, they are 18 (r) for the first group and 52 (Z) for
 - the second group. The sum of these is 70.
 -
 - Find the item type that corresponds to the badges of each three-Elf group.
 - What is the sum of the priorities of those item types?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.Set as Set
import qualified Data.Char as Char
import qualified Data.List as L
import qualified Control.Monad as M
import qualified System.Exit as Sys

main :: IO ()
main = defaultMain parseInput handleInput

newtype ElfGroup a = ElfGroup [Rucksack a] deriving (Show, Eq, Ord)
type Rucksack a = Set.Set a

handleInput :: [ElfGroup Char] -> IO ()
handleInput elves = case M.mapM findBadge elves of
    Left err -> Sys.die err
    Right badges -> print . sum . map priority $ badges

findBadge :: (Ord a, Show a) => ElfGroup a -> Either String a
findBadge (ElfGroup []) = Left "No badges in empty groups."
findBadge (ElfGroup elves) = case Set.toList found of
    [badge] -> Right badge
    [] -> Left $ "No badges found in group " ++ show elves ++ "."
    badges -> Left $ "Too many badges found " ++ show badges ++ " in "
        ++ show elves ++ "."
  where
    found = L.foldl1' Set.intersection elves

priority :: Char -> Word
priority x
    | Char.isLower x = fromIntegral $ Char.ord x - Char.ord 'a' + 1
    | otherwise = fromIntegral $ Char.ord x - Char.ord 'A' + 27

parseInput :: T.Text -> Either P.ParseError [ElfGroup Char]
parseInput = P.parse (parseElfGroups <* P.eof) ""

parseElfGroups :: P.Parsec T.Text () [ElfGroup Char]
parseElfGroups = P.many parseElfGroup

parseElfGroup :: P.Parsec T.Text () (ElfGroup Char)
parseElfGroup = ElfGroup <$> P.count 3 (parseRucksack <* P.newline)

parseRucksack :: P.Parsec T.Text () (Rucksack Char)
parseRucksack = Set.fromList <$> P.many (P.oneOf $ ['a'..'z'] ++ ['A'..'Z'])
