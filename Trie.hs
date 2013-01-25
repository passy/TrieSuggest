-- TODO: Use unicode Text thingy?

module Trie where

import ListUtils (addToAL)

data Trie a = Trie {
    value :: Maybe a,
    children :: [(Char, Trie a)]
} deriving (Show)

type DictTrie = Trie (String, Bool)

-- | Creates an empty Trie
emptyDictTrie :: Trie a
emptyDictTrie = Trie Nothing []

-- | Create a new Trie with a single string in it.
dictTrie :: String -> DictTrie
dictTrie s = emptyDictTrie { value = Just (s, False) }

-- TODO: Use cool Monad magic. liftM perhaps?
markLeaf :: Maybe (String, Bool) -> Maybe (String, Bool)
markLeaf v = case v of
    Just (a, _) -> Just (a, True)
    Nothing     -> Nothing

insert :: DictTrie -> String -> DictTrie
-- We exhausted the [Char] list
insert t []     = t { value = markLeaf $ value t }
insert t (x:xs) =
        let childNodes = children t
                               -- Create a new node with just the current char
                               -- in it
            newNode    = maybe (dictTrie [x])
                               -- Append the current key to the key of the
                               -- parent node.
                               (dictTrie . (++[x]) . fst)
                               (value t)
        -- Check if the current key already exists among the children
        in case lookup x childNodes of
            -- The key 'x' already exists in the current subtree.
            Just t' -> t { children = addToAL childNodes x (insert t' xs)}
            -- The key 'x' is unused among the subtree's children.
            Nothing -> t { children = childNodes ++ [(x, insert newNode xs)] }
