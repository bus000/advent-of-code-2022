{- Now, you just need to put all of the packets in the right order. Disregard
 - the blank lines in your list of received packets.
 -
 - The distress signal protocol also requires that you include two additional
 - divider packets:
 -
 -    [[2]]
 -    [[6]]
 -
 - Using the same rules as before, organize all packets - the ones in your list
 - of received packets as well as the two divider packets - into the correct
 - order.
 -
 - For the example above, the result of putting the packets in the correct order
 - is:
 -
 -    []
 -    [[]]
 -    [[[]]]
 -    [1,1,3,1,1]
 -    [1,1,5,1,1]
 -    [[1],[2,3,4]]
 -    [1,[2,[3,[4,[5,6,0]]]],8,9]
 -    [1,[2,[3,[4,[5,6,7]]]],8,9]
 -    [[1],4]
 -    [[2]]
 -    [3]
 -    [[4,4],4,4]
 -    [[4,4],4,4,4]
 -    [[6]]
 -    [7,7,7]
 -    [7,7,7,7]
 -    [[8,7,6]]
 -    [9]
 -
 - Afterward, locate the divider packets. To find the decoder key for this
 - distress signal, you need to determine the indices of the two divider packets
 - and multiply them together. (The first packet is at index 1, the second
 - packet is at index 2, and so on.) In this example, the divider packets are
 - 10th and 14th, and so the decoder key is 140.
 -
 - Organize all of the packets into the correct order. What is the decoder key
 - for the distress signal?
 -}
module Main where

import AdventOfCode
import qualified Data.List as L
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = defaultMain parseInput handleInput

data Packet = List ![Packet] | Term !Integer deriving (Show, Eq)

instance Ord Packet where
    compare (Term x) (Term y) = compare x y
    compare (Term x) (List y) = compare (List [Term x]) (List y)
    compare (List x) (Term y) = compare (List x) (List [Term y])
    compare (List x) (List y) = compare x y

handleInput :: [Packet] -> IO ()
handleInput packets
    = print
    . (product :: [Int] -> Int)
    . map fst
    . filter (isDivider . snd)
    . zip [1..]
    . L.sort
    $ divider1:divider2:packets
  where
    isDivider packet = packet == divider1 || packet == divider2
    divider1 = List [List [Term 2]]
    divider2 = List [List [Term 6]]

parseInput :: T.Text -> Either P.ParseError [Packet]
parseInput = P.parse (parsePackets <* P.eof) ""

parsePackets :: P.Parsec T.Text () [Packet]
parsePackets = parsePacket `P.endBy` P.many1 P.newline

parsePacket :: P.Parsec T.Text () Packet
parsePacket = parseList P.<|> parseTerm
  where
    parseList = List <$> P.between (P.char '[') (P.char ']')
        (parsePacket `P.sepBy` P.char ',')
    parseTerm = Term <$> P.int
