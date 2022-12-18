{- --- Part Two ---
 -
 - As you watch the crane operator expertly rearrange the crates, you notice the
 - process isn't following your prediction.
 -
 - Some mud was covering the writing on the side of the crane, and you quickly
 - wipe it away. The crane isn't a CrateMover 9000 - it's a CrateMover 9001.
 -
 - The CrateMover 9001 is notable for many new and exciting features: air
 - conditioning, leather seats, an extra cup holder, and the ability to pick up
 - and move multiple crates at once.
 -
 - Again considering the example above, the crates begin in the same
 - configuration:
 -
 -         [D]
 -     [N] [C]
 -     [Z] [M] [P]
 -      1   2   3 
 -
 - Moving a single crate from stack 2 to stack 1 behaves the same as before:
 -
 -     [D]        
 -     [N] [C]    
 -     [Z] [M] [P]
 -      1   2   3 
 -
 - However, the action of moving three crates from stack 1 to stack 3 means that
 - those three moved crates stay in the same order, resulting in this new
 - configuration:
 -
 -             [D]
 -             [N]
 -         [C] [Z]
 -         [M] [P]
 -      1   2   3
 -
 - Next, as both crates are moved from stack 2 to stack 1, they retain their
 - order as well:
 -
 -             [D]
 -             [N]
 -     [C]     [Z]
 -     [M]     [P]
 -      1   2   3
 -
 - Finally, a single crate is still moved from stack 1 to stack 2, but now it's
 - crate C that gets moved:
 -
 -             [D]
 -             [N]
 -             [Z]
 -     [M] [C] [P]
 -      1   2   3
 -
 - In this example, the CrateMover 9001 has put the crates in a totally
 - different order: MCD.
 -
 - Before the rearrangement process finishes, update your simulation so that the
 - Elves know where they should stand to be ready to unload the final supplies.
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

    let iAmount = fromIntegral amount
        fromStack' = drop iAmount fromStack
        toStack' = take iAmount fromStack ++ toStack

    return $ Map.insert to toStack' $ Map.insert from fromStack' stacks

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

    parseNothing = P.string "   " *> pure Nothing
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
