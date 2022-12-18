{- --- Day 5: Supply Stacks ---
 -
 - The expedition can depart as soon as the final supplies have been unloaded
 - from the ships. Supplies are stored in stacks of marked crates, but because
 - the needed supplies are buried under many other crates, the crates need to be
 - rearranged.
 -
 - The ship has a giant cargo crane capable of moving crates between stacks. To
 - ensure none of the crates get crushed or fall over, the crane operator will
 - rearrange them in a series of carefully-planned steps. After the crates are
 - rearranged, the desired crates will be at the top of each stack.
 -
 - The Elves don't want to interrupt the crane operator during this delicate
 - procedure, but they forgot to ask her which crate will end up where, and they
 - want to be ready to unload them as soon as possible so they can embark.
 -
 - They do, however, have a drawing of the starting stacks of crates and the
 - rearrangement procedure (your puzzle input). For example:
 -
 -        [D]
 -    [N] [C]
 -    [Z] [M] [P]
 -     1   2   3
 -
 -    move 1 from 2 to 1
 -    move 3 from 1 to 3
 -    move 2 from 2 to 1
 -    move 1 from 1 to 2
 -
 - In this example, there are three stacks of crates. Stack 1 contains two
 - crates: crate Z is on the bottom, and crate N is on top. Stack 2 contains
 - three crates; from bottom to top, they are crates M, C, and D. Finally, stack
 - 3 contains a single crate, P.
 -
 - Then, the rearrangement procedure is given. In each step of the procedure, a
 - quantity of crates is moved from one stack to a different stack. In the first
 - step of the above rearrangement procedure, one crate is moved from stack 2 to
 - stack 1, resulting in this configuration:
 -
 -     [D]
 -     [N] [C]
 -     [Z] [M] [P]
 -      1   2   3
 -
 - In the second step, three crates are moved from stack 1 to stack 3. Crates
 - are moved one at a time, so the first crate to be moved (D) ends up below the
 - second and third crates:
 -
 -            [Z]
 -            [N]
 -        [C] [D]
 -        [M] [P]
 -     1   2   3
 -
 - Then, both crates are moved from stack 2 to stack 1. Again, because crates
 - are moved one at a time, crate C ends up below crate M:
 -
 -            [Z]
 -            [N]
 -    [M]     [D]
 -    [C]     [P]
 -     1   2   3
 -
 - Finally, one crate is moved from stack 1 to stack 2:
 -
 -            [Z]
 -            [N]
 -            [D]
 -    [C] [M] [P]
 -     1   2   3
 -
 - The Elves just need to know which crate will end up on top of each stack; in
 - this example, the top crates are C in stack 1, M in stack 2, and Z in stack
 - 3, so you should combine these together and give the Elves the message CMZ.
 -
 - After the rearrangement procedure completes, what crate ends up on top of
 - each stack?
 -}
module Main where

import AdventOfCode
import qualified Control.Monad as M
import qualified Data.Text as T
import qualified Data.Maybe as Maybe
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import Text.Parsec ((<|>))
import qualified System.Exit as Sys

main :: IO ()
main = defaultMain parseInput handleInput

type Stack = String
type Stacks = Map.Map Char Stack

data Input = Input
    { _stacks    :: !Stacks
    , _movements :: ![Movement]
    } deriving (Show, Eq, Ord)

data Movement = Movement
    { _from   :: !Char
    , _to     :: !Char
    , _amount :: !Word
    } deriving (Show, Eq, Ord)

handleInput :: Input -> IO ()
handleInput (Input stacks movements) = case M.foldM move stacks movements of
    Left err -> Sys.die err
    Right resultStacks -> putStrLn . concatMap f $ resultStacks
  where
    f "" = ""
    f (c:_) = [c]

move :: Stacks -> Movement -> Either String Stacks
move stacks (Movement from to amount) = do
    fromStack <- findStack stacks from
    toStack <- findStack stacks to
    (fromStack', toStack') <- moveN fromStack toStack amount

    return $ Map.insert to toStack' $ Map.insert from fromStack' stacks

moveN :: [a] -> [a] -> Word -> Either String ([a], [a])
moveN from to 0 = Right (from, to)
moveN [] _ _ = Left "Not enough elements in source stack."
moveN (f:fs) ts amount = moveN fs (f:ts) (amount - 1)

findStack :: Stacks -> Char -> Either String Stack
findStack stacks name = case Map.lookup name stacks of
    Just stack -> Right stack
    Nothing -> Left $ "Could not find stack with name " ++ [name] ++ "."

parseInput :: T.Text -> Either P.ParseError Input
parseInput = P.parse parser ""
  where
    parser = Input
        <$> parseStacks <* P.string "\n\n"
        <*> parseMovements <* P.eof

parseStacks :: P.Parsec T.Text () Stacks
parseStacks = do
    ls <- P.try parseStackLine `P.endBy1` P.newline
    -- Ensure each line has same length.
    let lineLen = length . head $ ls
    M.guard $ all (\l -> length l == lineLen) ls

    let stacks = map (Maybe.catMaybes . reverse) . L.transpose . reverse $ ls

    stackNames <- parseStackNames
    -- Ensure we have 1 name per stack.
    M.guard $ length stackNames == length stacks
    return . Map.fromList $ zip stackNames stacks
  where
    parseStackLine :: P.Parsec T.Text () [Maybe Char]
    parseStackLine = parseStackEntry `P.sepBy` P.char ' '

    parseStackEntry :: P.Parsec T.Text () (Maybe Char)
    parseStackEntry = parseNothing <|> parseLetter

    parseNothing = P.string "   " $> pure Nothing
    parseLetter = Just <$> (P.char '[' *> P.alphaNum <* P.char ']')

    parseStackNames :: P.Parsec T.Text () String
    parseStackNames = spaces *> P.alphaNum `P.endBy` spaces

    spaces = P.many (P.char ' ')

parseMovements :: P.Parsec T.Text () [Movement]
parseMovements = parseMovement `P.endBy` P.newline

parseMovement :: P.Parsec T.Text () Movement
parseMovement = do
    _ <- P.string "move "
    amount <- P.int
    _ <- P.string " from "
    from <- P.alphaNum
    _ <- P.string " to "
    to <- P.alphaNum

    return $ Movement from to amount
