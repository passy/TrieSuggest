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
    sWords <- readFile "/usr/share/dict/words"
    let t = foldr (flip insert) emptyDictTrie $ lines sWords

    forever $ do
        putStr "suggest> "
        hFlush stdout
        getLine >>= (putStr . findSuggest t)
