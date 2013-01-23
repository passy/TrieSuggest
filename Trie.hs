module Trie where

data Trie a = Trie {
    value :: Maybe a,
    children :: [(Char, Trie a)]
}
