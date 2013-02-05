module Main (main) where

import Trie
import System.IO
import Control.Monad (forever)
import Data.Char (isSpace)


rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse


findSuggest :: DictTrie -> String -> String
findSuggest t s =
    case suggest t (rstrip s) of
        Just v -> unlines v
        Nothing -> "<not found>\n"

main :: IO ()
main = do
    let sWords = ["hello", "hell", "help", "holodeck"]
        t = foldr (flip insert) emptyDictTrie sWords

    forever $ do
        putStr "suggest> "
        hFlush stdout
        input <- getLine
        putStr $ findSuggest t input
