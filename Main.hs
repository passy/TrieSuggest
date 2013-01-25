module Main (main) where

import Trie

main :: IO ()
main = do
    let t = emptyDictTrie
        t' = insert t "hello"
        t'' = insert t' "hell"

    print t''
