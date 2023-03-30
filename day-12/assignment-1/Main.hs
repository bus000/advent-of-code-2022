{- --- Day 12: Hill Climbing Algorithm ---
 -
 - You try contacting the Elves using your handheld device, but the river you're
 - following must be too low to get a decent signal.
 -
 - You ask the device for a heightmap of the surrounding area (your puzzle
 - input). The heightmap shows the local area from above broken into a grid; the
 - elevation of each square of the grid is given by a single lowercase letter,
 - where a is the lowest elevation, b is the next-lowest, and so on up to the
 - highest elevation, z.
 -
 - Also included on the heightmap are marks for your current position (S) and
 - the location that should get the best signal (E). Your current position (S)
 - has elevation a, and the location that should get the best signal (E) has
 - elevation z.
 -
 - You'd like to reach E, but to save energy, you should do it in as few steps
 - as possible. During each step, you can move exactly one square up, down,
 - left, or right. To avoid needing to get out your climbing gear, the elevation
 - of the destination square can be at most one higher than the elevation of
 - your current square; that is, if your current elevation is m, you could step
 - to elevation n, but not to elevation o. (This also means that the elevation
 - of the destination square can be much lower than the elevation of your
 - current square.)
 -
 - For example:
 -
 -    Sabqponm
 -    abcryxxl
 -    accszExk
 -    acctuvwj
 -    abdefghi
 -
 - Here, you start in the top-left corner; your goal is near the middle. You
 - could start by moving down or right, but eventually you'll need to head
 - toward the e at the bottom. From there, you can spiral around to the goal:
 -
 -    v..v<<<<
 -    >v.vv<<^
 -    .>vv>E^^
 -    ..v>>>^^
 -    ..>>>>>^
 -
 - In the above diagram, the symbols indicate whether the path exits each square
 - moving up (^), down (v), left (<), or right (>). The location that should get
 - the best signal is still E, and . marks unvisited squares.
 -
 - This path reaches the goal in 31 steps, the fewest possible.
 -
 - What is the fewest steps required to move from your current position to the
 - location that should get the best signal?
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
import qualified System.Exit as Sys

main :: IO ()
main = defaultMain parseInput handleInput

data HeightMap = HeightMap
    { _heights :: !(A.Array Position Word)
    } deriving (Show, Eq, Ord)

type Position = (Int, Int)

data Input = Input
    { _map   :: !HeightMap
    , _start :: !Position
    , _goal  :: !Position
    } deriving (Show, Eq, Ord)

handleInput :: Input -> IO ()
handleInput (Input heightMap start goal) =
    case stepsToReach heightMap start goal of
        Just steps -> print steps
        Nothing -> Sys.die "No path from start to goal."

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

locateStart :: [[Char]] -> Maybe Position
locateStart = locate 'S'

locateGoal :: [[Char]] -> Maybe Position
locateGoal = locate 'E'

locate :: Eq a => a -> [[a]] -> Maybe Position
locate target elements = Maybe.listToMaybe $ do
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
            start <- locateStart charArray
            goal <- locateGoal charArray
            return $ Input heightMap start goal

        M.guard $ Maybe.isJust input
        return $ Maybe.fromJust input

parseCharArray :: P.Parsec T.Text () [[Char]]
parseCharArray = parseCharLine `P.endBy` P.newline

parseCharLine :: P.Parsec T.Text () [Char]
parseCharLine = P.many $ P.oneOf chars
  where
    chars = 'S':'E':['a'..'z']
