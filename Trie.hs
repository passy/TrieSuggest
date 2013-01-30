-- TODO: Use unicode Text thingy?

module Trie(
    Trie,
    insert,
    emptyDictTrie,
    dictTrie,
    findPrefix
) where

import ListUtils (addToAL)
import Control.Monad (liftM)
import Data.Maybe
import Data.Monoid (mempty, mappend, mconcat)
import qualified Data.Foldable as F

data Trie a = Trie {
    value :: Maybe a,
    children :: [(Char, Trie a)]
} deriving (Show)

{- | Convenience function to partially apply foldr on a subtrie, ignoring the
Char -}
tfold :: F.Foldable f => (a -> b -> b) -> (t, f a) -> b -> b
tfold f (_, a) b = F.foldr f b a

instance F.Foldable Trie where
    foldr f z (Trie (Just v) children) =
        F.foldr (tfold f) (f v z) children

    foldr f z (Trie Nothing children) =
        F.foldr (tfold f) z children

type DictTrie = Trie (String, Bool)

-- | Creates an empty Trie
emptyDictTrie :: Trie a
emptyDictTrie = Trie Nothing []

-- | Create a new Trie with a single string in it.
dictTrie :: String -> DictTrie
dictTrie s = emptyDictTrie { value = Just (s, False) }

markLeaf :: Maybe (String, Bool) -> Maybe (String, Bool)
markLeaf = liftM (\(s, _) -> (s, True))

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
            Just t' -> t { children = addToAL childNodes x $ insert t' xs}
            -- The key 'x' is unused among the subtree's children.
            Nothing -> t { children = childNodes ++ [(x, insert newNode xs)] }


-- | Find the prefix in the given trie and return the matching subtree, if it
-- exists.
findPrefix :: DictTrie -> String -> Maybe DictTrie
findPrefix t []     = Just t
findPrefix t (x:xs) = case lookup x $ children t of
                        Just t' -> findPrefix t' xs
                        Nothing -> Nothing
