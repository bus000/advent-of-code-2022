{- Now, you're ready to choose a directory to delete.
 -
 - The total disk space available to the filesystem is 70000000. To run the
 - update, you need unused space of at least 30000000. You need to find a
 - directory you can delete that will free up enough space to run the update.
 -
 - In the example above, the total size of the outermost directory (and thus the
 - total amount of used space) is 48381165; this means that the size of the
 - unused space must currently be 21618835, which isn't quite the 30000000
 - required by the update. Therefore, the update still requires a directory with
 - total size of at least 8381165 to be deleted before it can run.
 -
 - To achieve this, you have the following options:
 -
 - * Delete directory e, which would increase unused space by 584.
 - * Delete directory a, which would increase unused space by 94853.
 - * Delete directory d, which would increase unused space by 24933642.
 - * Delete directory /, which would increase unused space by 48381165.
 -
 - Directories e and a are both too small; deleting them would not free up
 - enough space. However, directories d and / are both big enough! Between
 - these, choose the smallest: d, increasing unused space by 24933642.
 -
 - Find the smallest directory that, if deleted, would free up enough space on
 - the filesystem to run the update. What is the total size of that directory?
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.List as L
import Data.List (foldl')
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import Control.Applicative ((<|>))
import qualified System.Exit as Sys

main :: IO ()
main = defaultMain parseInput handleInput

data Command = LS ![Entry] | CD !T.Text deriving (Show, Eq, Ord)

data Entry = File !Word !T.Text | Directory !T.Text deriving (Show, Eq, Ord)

newtype FileSystem = FileSystem
    { _files :: Map.Map [T.Text] Word
    } deriving (Show, Eq, Ord)

data Shell = Shell
    { _wd   :: ![T.Text]   -- Working directory.
    , _root :: !FileSystem -- File system.
    } deriving (Show, Eq, Ord)

handleInput :: [Command] -> IO ()
handleInput commands = case freeSpace dirs of
    Just size -> print size
    Nothing -> Sys.die "Could not find any files matching requirements."
  where
    dirs = directories . buildFileSystem $ commands

freeSpace :: Map.Map T.Text Word -> Maybe Word
freeSpace files = do
    used <- Map.lookup "" files
    let requiredSize = requiredSpace - (size - used)
        sizes = map snd . Map.toList $ files
        candidates = filter (>= requiredSize) sizes
    if null candidates
        then Nothing
        else return $ minimum candidates
  where
    requiredSpace = 30000000
    size = 70000000

buildFileSystem :: [Command] -> FileSystem
buildFileSystem = _root . foldl' go (Shell [] emptyFileSystem)
  where
    go shell (LS entries) = foldl' addEntry shell entries
    go shell (CD "/") = shell { _wd = [] }
    go shell@(Shell [] _) (CD "..") = shell
    go (Shell (_:wd) root) (CD "..") = Shell wd root
    go shell (CD dir) = shell { _wd = dir:_wd shell }

directories :: FileSystem -> Map.Map T.Text Word
directories (FileSystem files)
    = Map.fromListWith (+)
    . map dirName
    . concatMap allDirs
    . Map.toList
    $ files
  where
    allDirs (fileName, size) = map (,size) . tail . L.tails $ fileName
    dirName (dirNames, size) = (T.concat . L.intersperse "/" $ dirNames, size)

emptyFileSystem :: FileSystem
emptyFileSystem = FileSystem Map.empty

addEntry :: Shell -> Entry -> Shell
addEntry shell (Directory _) = shell
addEntry shell (File size name) = shell { _root = root' }
  where
    files = _files . _root $ shell
    files' = Map.insert (name:_wd shell) size files
    root' = FileSystem files'

parseInput :: T.Text -> Either P.ParseError [Command]
parseInput = P.parse (parseCommands <* P.eof) ""

parseCommands :: P.Parsec T.Text () [Command]
parseCommands = P.many parseCommand

parseCommand :: P.Parsec T.Text () Command
parseCommand = P.string "$ " *> (parseCd <|> parseLs)
  where
    parseCd = CD . T.pack <$>
        (P.string "cd " *> P.many (P.noneOf "\n") <* P.newline)
    parseLs = LS <$> (P.string "ls\n" *> lsOuts)

    lsOuts = lsOut `P.endBy` P.newline
    lsOut = folder <|> file

    folder = Directory . T.pack <$>
        (P.string "dir " *> P.many (P.noneOf "\n"))
    file = File <$> P.int <* P.space <*> (T.pack <$> P.many (P.noneOf "\n"))
