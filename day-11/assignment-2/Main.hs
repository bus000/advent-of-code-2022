{- You're worried you might not ever get your items back. So worried, in fact,
 - that your relief that a monkey's inspection didn't damage an item no longer
 - causes your worry level to be divided by three.
 -
 - Unfortunately, that relief was all that was keeping your worry levels from
 - reaching ridiculous levels. You'll need to find another way to keep your
 - worry levels manageable.
 -
 - At this rate, you might be putting up with these monkeys for a very long
 - time - possibly 10000 rounds!
 -
 - With these new rules, you can still figure out the monkey business after
 - 10000 rounds. Using the same example above:
 -
 -    == After round 1 ==
 -    Monkey 0 inspected items 2 times.
 -    Monkey 1 inspected items 4 times.
 -    Monkey 2 inspected items 3 times.
 -    Monkey 3 inspected items 6 times.
 -
 -    == After round 20 ==
 -    Monkey 0 inspected items 99 times.
 -    Monkey 1 inspected items 97 times.
 -    Monkey 2 inspected items 8 times.
 -    Monkey 3 inspected items 103 times.
 -
 -    == After round 1000 ==
 -    Monkey 0 inspected items 5204 times.
 -    Monkey 1 inspected items 4792 times.
 -    Monkey 2 inspected items 199 times.
 -    Monkey 3 inspected items 5192 times.
 -
 -    == After round 2000 ==
 -    Monkey 0 inspected items 10419 times.
 -    Monkey 1 inspected items 9577 times.
 -    Monkey 2 inspected items 392 times.
 -    Monkey 3 inspected items 10391 times.
 -
 -    == After round 3000 ==
 -    Monkey 0 inspected items 15638 times.
 -    Monkey 1 inspected items 14358 times.
 -    Monkey 2 inspected items 587 times.
 -    Monkey 3 inspected items 15593 times.
 -
 -    == After round 4000 ==
 -    Monkey 0 inspected items 20858 times.
 -    Monkey 1 inspected items 19138 times.
 -    Monkey 2 inspected items 780 times.
 -    Monkey 3 inspected items 20797 times.
 -
 -    == After round 5000 ==
 -    Monkey 0 inspected items 26075 times.
 -    Monkey 1 inspected items 23921 times.
 -    Monkey 2 inspected items 974 times.
 -    Monkey 3 inspected items 26000 times.
 -
 -    == After round 6000 ==
 -    Monkey 0 inspected items 31294 times.
 -    Monkey 1 inspected items 28702 times.
 -    Monkey 2 inspected items 1165 times.
 -    Monkey 3 inspected items 31204 times.
 -
 -    == After round 7000 ==
 -    Monkey 0 inspected items 36508 times.
 -    Monkey 1 inspected items 33488 times.
 -    Monkey 2 inspected items 1360 times.
 -    Monkey 3 inspected items 36400 times.
 -
 -    == After round 8000 ==
 -    Monkey 0 inspected items 41728 times.
 -    Monkey 1 inspected items 38268 times.
 -    Monkey 2 inspected items 1553 times.
 -    Monkey 3 inspected items 41606 times.
 -
 -    == After round 9000 ==
 -    Monkey 0 inspected items 46945 times.
 -    Monkey 1 inspected items 43051 times.
 -    Monkey 2 inspected items 1746 times.
 -    Monkey 3 inspected items 46807 times.
 -
 -    == After round 10000 ==
 -    Monkey 0 inspected items 52166 times.
 -    Monkey 1 inspected items 47830 times.
 -    Monkey 2 inspected items 1938 times.
 -    Monkey 3 inspected items 52013 times.
 -
 - After 10000 rounds, the two most active monkeys inspected items 52166 and
 - 52013 times. Multiplying these together, the level of monkey business in this
 - situation is now 2713310158.
 -
 - Worry levels are no longer divided by three after each item is inspected;
 - you'll need to find another way to keep your worry levels manageable.
 - Starting again from the initial state in your puzzle input, what is the level
 - of monkey business after 10000 rounds?
 -}
{-# LANGUAGE LambdaCase #-}
module Main where

import AdventOfCode
import qualified Data.Queue as Q
import qualified Control.Monad as M
import qualified Control.Monad.State as M
import qualified Control.Monad.Except as M
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import qualified Text.Parsec.Number as P
import qualified System.Exit as Sys
import qualified Data.List as L

main :: IO ()
main = defaultMain parseInput handleInput

data Monkey = Monkey
    { _name            :: !Name
    , _items           :: !(Q.Queue Remainders)
    , _inspection      :: Remainders -> Remainders
    , _test            :: Remainders -> Bool
    , _trueAction      :: !Word
    , _falseAction     :: !Word
    , _inspectionCount :: !Word
    }

newtype Remainders = Remainders [Word] deriving (Show, Eq, Ord)

instance Show Monkey where
    show (Monkey name items _ _ true false inspectionCount) =
        "Monkey (" ++ show name ++ ") with items " ++ show items
            ++ " throwing to " ++ show true ++ " and " ++ show false
            ++ ". Has inspected " ++ show inspectionCount ++ " items."

type Name = Word
type Troop = Map.Map Name Monkey
type MonkeyBusiness a = M.ExceptT String (M.State Troop) a

handleInput :: Troop -> IO ()
handleInput troop = case simulate 10000 troop of
    Right out -> print . handleOut $ out
    Left err -> Sys.die err
  where
    handleOut
        = product
        . take 2
        . L.sortBy (flip compare)
        . map _inspectionCount
        . Map.elems

-- Simulate n steps of the monkey business.
simulate :: Word -> Troop -> Either String Troop
simulate n = M.evalState (M.runExceptT (go n))
  where
    go 0 = M.get
    go it = doRound >> go (it - 1)

-- Loop through monkeys and throw all items.
doRound :: MonkeyBusiness ()
doRound = do
    troop <- M.lift M.get
    let monkeys = Map.keys troop
    M.mapM_ throwItems monkeys

throwItems :: Name -> MonkeyBusiness ()
throwItems name = do
    monkey <- findMonkey name
    if null (_items monkey)
        then return ()
        else throwItem name >> throwItems name

throwItem :: Name -> MonkeyBusiness ()
throwItem name = do
    item <- popItem name
    monkey <- findMonkey name
    let inspected = _inspection monkey item
        monkey' = incrementInspections monkey
        target = if _test monkey inspected
            then _trueAction monkey
            else _falseAction monkey

    pushItem target inspected
    saveMonkey monkey'

findMonkey :: Name -> MonkeyBusiness Monkey
findMonkey name = do
    troop <- M.get
    case Map.lookup name troop of
        Just monkey -> return monkey
        Nothing -> monkeyError $ "Could not find monkey " ++ show name ++ "."

modifyMonkey :: (Monkey -> (Monkey, a)) -> Name -> MonkeyBusiness a
modifyMonkey f name = do
    monkey <- findMonkey name
    let (monkey', result) = f monkey
    saveMonkey monkey'
    return result

saveMonkey :: Monkey -> MonkeyBusiness ()
saveMonkey monkey = M.lift (M.modify $ \troop -> Map.insert name monkey troop)
  where
    name = _name monkey

popItem :: Name -> MonkeyBusiness Remainders
popItem name = modifyMonkey pop name >>= \case
    Just item -> return item
    Nothing -> monkeyError $
        "Monkey " ++ show name ++ " does not have any items."
  where
    pop monkey = case Q.dequeue (_items monkey) of
        (_, Nothing) -> (monkey, Nothing)
        (items, Just item) -> (monkey { _items = items }, Just item)

pushItem :: Name -> Remainders -> MonkeyBusiness ()
pushItem name item = modifyMonkey push name
  where
    push monkey = (monkey { _items = Q.enqueue item (_items monkey) }, ())

monkeyError :: String -> MonkeyBusiness a
monkeyError = M.throwError

incrementInspections :: Monkey -> Monkey
incrementInspections monkey = monkey
    { _inspectionCount = _inspectionCount monkey + 1 }

newRemainders :: Word -> Remainders
newRemainders n = Remainders [n `mod` i | i <- [1..]]

add :: Remainders -> Word -> Remainders
add (Remainders remainders) x = Remainders $ zipWith go [1..] remainders
  where
    go i n = (n + x) `mod` i

mul :: Remainders -> Word -> Remainders
mul (Remainders remainders) x = Remainders $ zipWith go [1..] remainders
  where
    go i n = (n * x) `mod` i

square :: Remainders -> Remainders
square (Remainders remainders) = Remainders $ zipWith go [1..] remainders
  where
    go i n = (n * n) `mod` i

double :: Remainders -> Remainders
double (Remainders remainders) = Remainders $ zipWith go [1..] remainders
  where
    go i n = (n + n) `mod` i

divisible :: Remainders -> Word -> Bool
divisible (Remainders remainders) n = remainders !! ix == 0
  where
    ix = fromIntegral $ n - 1

parseInput :: T.Text -> Either P.ParseError Troop
parseInput = P.parse (parseTroop <* P.eof) ""

parseTroop :: P.Parsec T.Text () Troop
parseTroop = Map.fromList . map extractName <$> parseMonkeys
  where
    extractName monkey = (_name monkey, monkey)

parseMonkeys :: P.Parsec T.Text () [Monkey]
parseMonkeys = parseMonkey `P.sepBy` P.newline

parseMonkey :: P.Parsec T.Text () Monkey
parseMonkey = do
    _ <- P.string "Monkey "
    name <- P.int
    _ <- P.string ":\n  Starting items: "
    items <- Q.fromList . map newRemainders <$> P.int `P.sepBy` P.string ", "
    _ <- P.string "\n  Operation: new = "
    operation <- parseOperation
    _ <- P.string "\n  Test: "
    test <- parseTest
    _ <- P.string "\n    If true: throw to monkey "
    true <- P.int
    _ <- P.string "\n    If false: throw to monkey "
    false <- P.int
    _ <- P.newline

    return $ Monkey name items operation test true false 0

parseOperation :: P.Parsec T.Text () (Remainders -> Remainders)
parseOperation = P.string "old " *> (times <|> plus)
  where
    times = timesOp <$> (P.string "* " *> parseTarget)
    plus = plusOp <$> (P.string "+ " *> parseTarget)

    timesOp Nothing = square
    timesOp (Just x) = flip mul x

    plusOp Nothing = double
    plusOp (Just x) = flip add x

    parseTarget = (P.string "old" *> pure Nothing) <|> (Just <$> P.int)

parseTest :: P.Parsec T.Text () (Remainders -> Bool)
parseTest = test <$> (P.string "divisible by " *> P.int)
  where
    test x y = divisible y x
