{- You realize you misread the scan. There isn't an endless void at the bottom
 - of the scan - there's floor, and you're standing on it!
 -
 - You don't have time to scan the floor, so assume the floor is an infinite
 - horizontal line with a y coordinate equal to two plus the highest y
 - coordinate of any point in your scan.
 -
 - In the example above, the highest y coordinate of any point is 9, and so the
 - floor is at y=11. (This is as if your scan contained one extra rock path like
 - -infinity,11 -> infinity,11.) With the added floor, the example above now
 - looks like this:
 -
 -            ...........+........
 -            ....................
 -            ....................
 -            ....................
 -            .........#...##.....
 -            .........#...#......
 -            .......###...#......
 -            .............#......
 -            .............#......
 -            .....#########......
 -            ....................
 -    <-- etc #################### etc -->
 -
 - To find somewhere safe to stand, you'll need to simulate falling sand until a
 - unit of sand comes to rest at 500,0, blocking the source entirely and
 - stopping the flow of sand into the cave. In the example above, the situation
 - finally looks like this after 93 units of sand come to rest:
 -
 -    ............o............
 -    ...........ooo...........
 -    ..........ooooo..........
 -    .........ooooooo.........
 -    ........oo#ooo##o........
 -    .......ooo#ooo#ooo.......
 -    ......oo###ooo#oooo......
 -    .....oooo.oooo#ooooo.....
 -    ....oooooooooo#oooooo....
 -    ...ooo#########ooooooo...
 -    ..ooooo.......ooooooooo..
 -    #########################
 -
 - Using your scan, simulate the falling sand until the source of the sand
 - becomes blocked. How many units of sand come to rest?
 -}
module Main where

import AdventOfCode
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = defaultMain parseInput handleInput

type Position = (Int, Int)

data Tile = Sand | Rock deriving (Show, Eq, Ord)

data Slice = Slice !(Map.Map Position Tile) deriving (Show, Eq, Ord)

data DrawableWall = DrawableWall
    { _points :: ![Position] -- List of edges to draw lines between.
    } deriving (Show, Eq, Ord)

handleInput :: [DrawableWall] -> IO ()
handleInput = print . countSand . dropSand (500, 0) . buildSlice
  where
    countSand (Slice slice) = length . filter (== Sand) . Map.elems $ slice

{- Drop sand from the given position and simulate where the sand ends up. -}
dropSand :: Position -> Slice -> Slice
dropSand start (Slice startSlice)
    | Map.null startSlice = Slice startSlice
    | otherwise = Slice $ go start startSlice
  where
    maxY = maximum . map snd . Map.keys $ startSlice
    go pos slice
        | isSolid slice pos = slice
        | otherwise =
            makeSolid pos . dropRight pos . dropLeft pos . dropDown pos $ slice

    dropDown pos slice = go (down pos) slice
    dropLeft pos slice
        | isSolid slice (down pos) = go (downLeft pos) slice
        | otherwise = slice
    dropRight pos slice
        | isSolid slice (down pos) && isSolid slice (downLeft pos)
            = go (downRight pos) slice
        | otherwise = slice
    makeSolid pos slice
        | isSolid slice (down pos)
            && isSolid slice (downLeft pos)
            && isSolid slice (downRight pos) = Map.insert pos Sand slice
        | otherwise = slice

    isSolid slice (x, y) =
        y >= maxY + 2 || (x, y) `Map.member` slice
    down (x, y) = (x, y + 1)
    downLeft (x, y) = (x - 1, y + 1)
    downRight (x, y) = (x + 1, y + 1)

buildSlice :: [DrawableWall] -> Slice
buildSlice walls = Slice $ foldr addRock Map.empty rocks
  where
    rocks = concatMap (drawLine . _points) walls
    addRock rock slice = Map.insert rock Rock slice

{- Draw the lines between the points given. -}
drawLine :: [Position] -> [Position]
drawLine [] = []
drawLine (x:[]) = [x]
drawLine ((x1, y1):p2@(x2, y2):xs)
    | x1 < x2 = (x1, y1):drawLine ((x1 + 1, y1):p2:xs)
    | x1 > x2 = (x1, y1):drawLine ((x1 - 1, y1):p2:xs)
    | y1 < y2 = (x1, y1):drawLine ((x1, y1 + 1):p2:xs)
    | y1 > y2 = (x1, y1):drawLine ((x1, y1 - 1):p2:xs)
    | otherwise = drawLine (p2:xs)

parseInput :: T.Text -> Either P.ParseError [DrawableWall]
parseInput = P.parse (parseDrawableWalls <* P.eof) ""

parseDrawableWalls :: P.Parsec T.Text () [DrawableWall]
parseDrawableWalls = parseDrawableWall `P.endBy` P.newline

parseDrawableWall :: P.Parsec T.Text () DrawableWall
parseDrawableWall = DrawableWall
    <$> parsePosition `P.sepBy` P.string " -> "

parsePosition :: P.Parsec T.Text () Position
parsePosition = (,) <$> P.int <* P.char ',' <*> P.int
