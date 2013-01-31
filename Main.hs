module Main (main) where

import Trie

main :: IO ()
main = do
    let t = emptyDictTrie
        t' = insert t "hello"
        t'' = insert t' "hell"
        t''' = insert t'' "help"
        t'''' = insert t''' "holodeck"
        suggestions = suggest t'''' "hell"

    case suggestions of
        Just v -> print $ unwords v
        Nothing -> return ()
