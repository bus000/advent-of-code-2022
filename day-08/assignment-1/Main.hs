{- --- Day 8: Treetop Tree House ---
 -
 - The expedition comes across a peculiar patch of tall trees all planted
 - carefully in a grid. The Elves explain that a previous expedition planted
 - these trees as a reforestation effort. Now, they're curious if this would be
 - a good location for a tree house.
 -
 - First, determine whether there is enough tree cover here to keep a tree house
 - hidden. To do this, you need to count the number of trees that are visible
 - from outside the grid when looking directly along a row or column.
 -
 - The Elves have already launched a quadcopter to generate a map with the
 - height of each tree (your puzzle input). For example:
 -
 -     30373
 -     25512
 -     65332
 -     33549
 -     35390
 -
 - Each tree is represented as a single digit whose value is its height, where 0
 - is the shortest and 9 is the tallest.
 -
 - A tree is visible if all of the other trees between it and an edge of the
 - grid are shorter than it. Only consider trees in the same row or column; that
 - is, only look up, down, left, or right from any given tree.
 -
 - All of the trees around the edge of the grid are visible - since they are
 - already on the edge, there are no trees to block the view. In this example,
 - that only leaves the interior nine trees to consider:
 -
 - * The top-left 5 is visible from the left and top. (It isn't visible from the
 -   right or bottom since other trees of height 5 are in the way.)
 - * The top-middle 5 is visible from the top and right.
 - * The top-right 1 is not visible from any direction; for it to be visible,
 -   there would need to only be trees of height 0 between it and an edge.
 - * The left-middle 5 is visible, but only from the right.
 - * The center 3 is not visible from any direction; for it to be visible, there
 -   would need to be only trees of at most height 2 between it and an edge.
 - * The right-middle 3 is visible from the right.
 - * In the bottom row, the middle 5 is visible, but the 3 and 4 are not.
 -
 - With 16 trees visible on the edge and another 5 visible in the interior, a
 - total of 21 trees are visible in this arrangement.
 -
 - Consider your map; how many trees are visible from outside the grid?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Data.Array as A
import qualified Text.Parsec as P
import qualified Data.Maybe as Maybe

main :: IO ()
main = defaultMain parseInput handleInput

type Height = Word

newtype HeightMap a = HeightMap
    { _map :: A.Array (Int, Int) a
    } deriving (Show, Eq, Ord)

type Trees = HeightMap Height

instance Functor HeightMap where
    fmap f = HeightMap . fmap f . _map

handleInput :: Trees -> IO ()
handleInput = print . length . filter id . A.elems . observeTrees

observeTrees :: Trees -> A.Array (Int, Int) Bool
observeTrees (HeightMap trees) = A.array bounds $ do
    row <- [fromRow..toRow]
    column <- [fromCol..toCol]

    let tree = trees A.! (row, column)
        up = [trees A.! (i, column) | i <- [fromRow..(row - 1)]]
        down = [trees A.! (i, column) | i <- [(row + 1)..toRow]]
        left = [trees A.! (row, i) | i <- [fromCol..(column - 1)]]
        right = [trees A.! (row, i) | i <- [(column + 1)..toCol]]
        seen = largest tree up || largest tree down || largest tree left
            || largest tree right

    return ((row, column), seen)
  where
    bounds@((fromRow, fromCol), (toRow, toCol)) = A.bounds trees

    largest x = all (< x)

fromRows :: [[a]] -> Maybe (HeightMap a)
fromRows [] = Nothing
fromRows rows = Just . HeightMap . A.array (start, end) $ do
    (row, rowN) <- zip rows [0..]
    (element, colN) <- zip row [0..]

    return ((rowN, colN), element)
  where
    start = (0, 0)
    end = (length rows - 1, (length . head $ rows) - 1)

parseInput :: T.Text -> Either P.ParseError Trees
parseInput = P.parse (parseHeightMap <* P.eof) ""

parseHeightMap :: P.Parsec T.Text () Trees
parseHeightMap = fmap f . Maybe.fromJust . fromRows <$> parseRows
  where
    f '0' = 0
    f '1' = 1
    f '2' = 2
    f '3' = 3
    f '4' = 4
    f '5' = 5
    f '6' = 6
    f '7' = 7
    f '8' = 8
    f '9' = 9
    f err = error $ "Expected digit. Was '" ++ [err] ++ "'."

parseRows :: P.Parsec T.Text () [String]
parseRows = parseRow `P.endBy1` P.newline

parseRow :: P.Parsec T.Text () String
parseRow = P.many1 P.digit
