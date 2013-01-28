module Main (main) where

import Trie

main :: IO ()
main = do
    let t = emptyDictTrie
        t' = insert t "hello"
        t'' = insert t' "hell"
        t''' = insert t'' "help"

    print $ findPrefix t''' "hell"
