module Main (main) where

import Trie
import System.Environment


doSuggest :: DictTrie -> String -> IO ()
doSuggest t s = do
    case suggest t s of
        Just v -> putStrLn $ unlines v
        Nothing -> return ()

main :: IO ()
main = do
    let sWords = ["hello", "hell", "help", "holodeck"]
        t = foldr (flip insert) emptyDictTrie sWords

    args <- getArgs

    if length args < 1
        then print "Please provide a suggestion prefix!"
        else doSuggest t (args !! 0)
