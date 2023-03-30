{- As you walk up the hill, you suspect that the Elves will want to turn this
 - into a hiking trail. The beginning isn't very scenic, though; perhaps you can
 - find a better starting point.
 -
 - To maximize exercise while hiking, the trail should start as low as possible:
 - elevation a. The goal is still the square marked E. However, the trail should
 - still be direct, taking the fewest steps to reach its goal. So, you'll need
 - to find the shortest path from any square at elevation a to the square marked
 - E.
 -
 - Again consider the example from above:
 -
 -    Sabqponm
 -    abcryxxl
 -    accszExk
 -    acctuvwj
 -    abdefghi
 -
 - Now, there are six choices for starting position (five marked a, plus the
 - square marked S that counts as being at elevation a). If you start at the
 - bottom-left square, you can reach the goal most quickly:
 -
 -    ...v<<<<
 -    ...vv<<^
 -    ...v>E^^
 -    .>v>>>^^
 -    >^>>>>>^
 -
 - This path reaches the goal in only 29 steps, the fewest possible.
 -
 - What is the fewest steps required to move starting from any square with
 - elevation a to the location that should get the best signal?
 -}
{-# LANGUAGE TupleSections #-}
module Main where

import AdventOfCode
import qualified Algorithms.Dijkstra as Dijkstra
import qualified Control.Monad as M
import qualified Data.Array as A
import qualified Data.Char as C
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Text.Parsec as P

main :: IO ()
main = defaultMain parseInput handleInput

data HeightMap = HeightMap
    { _heights :: !(A.Array Position Word)
    } deriving (Show, Eq, Ord)

type Position = (Int, Int)

data Input = Input
    { _map    :: !HeightMap
    , _starts :: ![Position]
    , _goal   :: !Position
    } deriving (Show, Eq, Ord)

handleInput :: Input -> IO ()
handleInput (Input heightMap starts goal)
    = print
    . minimum
    . Maybe.catMaybes
    . map (\start -> stepsToReach heightMap start goal)
    $ starts

{- Compute the number of steps needed to get from first position to second in
 - the heightmap. -}
stepsToReach :: HeightMap -> Position -> Position -> Maybe Word
stepsToReach heightMap start goal = fmap snd . findGoal $ distances
  where
    distances = Dijkstra.dijkstra start neighboursWithCost
    neighboursWithCost = map (, 1) . neighbours heightMap
    findGoal = Maybe.listToMaybe . filter (\x -> fst x == goal)

neighbours :: HeightMap -> Position -> [Position]
neighbours (HeightMap heights) (x, y)
    = filter noClimb
    . filter inBound
    $ theNeighbours
  where
    ((xMin, yMin), (xMax, yMax)) = A.bounds heights
    theNeighbours =
        [                 (x + 0, y - 1)
        , (x - 1, y + 0),                (x + 1, y + 0)
        ,                 (x + 0, y + 1)
        ]
    noClimb neighbour = heights A.! neighbour <= heights A.! (x, y) + 1
    inBound (xt, yt) = xt >= xMin && xt <= xMax && yt >= yMin && yt <= yMax

fromCharArray :: [[Char]] -> Maybe HeightMap
fromCharArray [] = Nothing
fromCharArray chars = Just . HeightMap . A.array bounds $ do
    (y, line) <- zip [0..] chars
    (x, element) <- zip [0..] line

    let height = case element of
            'S' -> 0
            'E' -> C.ord 'z' - C.ord 'a'
            char -> C.ord char - C.ord 'a'

    return $ ((x, y), fromIntegral height)
  where
    bounds = (lower, upper)
    lower = (0, 0)
    upper = ((length . head $ chars) - 1, length chars - 1)

locateStarts :: [[Char]] -> [Position]
locateStarts chars = locateAll 'S' chars ++ locateAll 'a' chars

locateGoal :: [[Char]] -> Maybe Position
locateGoal = Maybe.listToMaybe . locateAll 'E'

locateAll :: Eq a => a -> [[a]] -> [Position]
locateAll target elements = do
    (y, line) <- zip [0..] elements
    (x, element) <- zip [0..] line
    M.guard $ element == target
    return (x, y)

parseInput :: T.Text -> Either P.ParseError Input
parseInput = P.parse (parseTheInput <* P.eof) ""
  where
    parseTheInput = do
        charArray <- parseCharArray
        let input = do
            heightMap <- fromCharArray charArray
            let starts = locateStarts charArray
            goal <- locateGoal charArray
            return $ Input heightMap starts goal

        M.guard $ Maybe.isJust input
        return $ Maybe.fromJust input

parseCharArray :: P.Parsec T.Text () [[Char]]
parseCharArray = parseCharLine `P.endBy` P.newline

parseCharLine :: P.Parsec T.Text () [Char]
parseCharLine = P.many $ P.oneOf chars
  where
    chars = 'S':'E':['a'..'z']
