module Main (main) where

import Trie

main :: IO ()
main = do
    let sWords = ["hello", "hell", "help", "holodeck"]
        t = foldr (flip insert) emptyDictTrie sWords

    case suggest t "hell" of
        Just v -> putStrLn $ unlines v
        Nothing -> return ()
