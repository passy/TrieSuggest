module Main (main) where

import Trie
import qualified Data.Foldable as F

main :: IO ()
main = do
    let t = emptyDictTrie
        t' = insert t "hello"
        t'' = insert t' "hell"
        t''' = insert t'' "help"

    print $ F.toList t'''
