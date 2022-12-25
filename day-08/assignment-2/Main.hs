{- Content with the amount of tree cover available, the Elves just need to know
 - the best spot to build their tree house: they would like to be able to see a
 - lot of trees.
 -
 - To measure the viewing distance from a given tree, look up, down, left, and
 - right from that tree; stop if you reach an edge or at the first tree that is
 - the same height or taller than the tree under consideration. (If a tree is
 - right on the edge, at least one of its viewing distances will be zero.)
 -
 - The Elves don't care about distant trees taller than those found by the rules
 - above; the proposed tree house has large eaves to keep it dry, so they
 - wouldn't be able to see higher than the tree house anyway.
 -
 - In the example above, consider the middle 5 in the second row:
 -
 -     30373
 -     25512
 -     65332
 -     33549
 -     35390
 -
 - * Looking up, its view is not blocked; it can see 1 tree (of height 3).
 - * Looking left, its view is blocked immediately; it can see only 1 tree (of
 -   height 5, right next to it).
 - * Looking right, its view is not blocked; it can see 2 trees.
 - * Looking down, its view is blocked eventually; it can see 2 trees (one of
 -   height 3, then the tree of height 5 that blocks its view).
 -
 - A tree's scenic score is found by multiplying together its viewing distance
 - in each of the four directions. For this tree, this is 4 (found by
 - multiplying 1 * 1 * 2 * 2).
 -
 - However, you can do even better: consider the tree of height 5 in the middle
 - of the fourth row:
 -
 -     30373
 -     25512
 -     65332
 -     33549
 -     35390
 -
 - * Looking up, its view is blocked at 2 trees (by another tree with a height
 -   of 5).
 - * Looking left, its view is not blocked; it can see 2 trees.
 - * Looking down, its view is also not blocked; it can see 1 tree.
 - * Looking right, its view is blocked at 2 trees (by a massive tree of height
 -   9).
 -
 - This tree's scenic score is 8 (2 * 2 * 1 * 2); this is the ideal spot for the
 - tree house.
 -
 - Consider each tree on your map. What is the highest scenic score possible for
 - any tree?
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
handleInput = print . maximum . A.elems . scenicScore

scenicScore :: Trees -> A.Array (Int, Int) Int
scenicScore (HeightMap trees) = A.array bounds $ do
    row <- [fromRow..toRow]
    column <- [fromCol..toCol]

    let tree = trees A.! (row, column)
        up = reverse [trees A.! (i, column) | i <- [fromRow..(row - 1)]]
        down = [trees A.! (i, column) | i <- [(row + 1)..toRow]]
        left = reverse [trees A.! (row, i) | i <- [fromCol..(column - 1)]]
        right = [trees A.! (row, i) | i <- [(column + 1)..toCol]]
        score = prefixSmaller tree up * prefixSmaller tree down
            * prefixSmaller tree left * prefixSmaller tree right

    return ((row, column), score)
  where
    bounds@((fromRow, fromCol), (toRow, toCol)) = A.bounds trees

    prefixSmaller x = length . takeWhileOneMore (< x)

    takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

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
