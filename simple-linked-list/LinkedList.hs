------------------------------------------------------------
---- |
---- Module: LinkedList
---- Description: Linked list (LIFO) implementation
---- Copyright: (c) 2015 Alex Dzyoba <alex.dzyoba@gmail.com>
---- License: MIT
------------------------------------------------------------

module LinkedList (
    nil, isNil,
    new, next, datum,
    toList, fromList,
    reverseLinkedList
) where

data List a = Nil | Cons a (List a)
    deriving (Show, Eq, Ord)

-- | Add new element to a list
new :: a -> List a -> List a
new x Nil = Cons x Nil
new x (Cons a next) = Cons x (Cons a next)

-- | Check if list is empty
isNil :: List a -> Bool
isNil Nil = True
isNil (Cons a next) = False

-- | Return empty list
nil :: List a
nil = Nil

-- | Return head of the list
datum :: List a -> a
datum Nil = error "Empty list"
datum (Cons a next) = a

-- | Return next element in a list
next :: List a -> List a
next Nil = error "Empty list"
next (Cons a next) = next

-- | Convert LinkedList to prelude's list
toList :: List a -> [a]
toList Nil = []
toList (Cons a next) = a : toList next

-- | Convert prelude's list to LinkedList
-- Thanks, hlint!
fromList :: [a] -> List a
fromList = foldr Cons Nil

-- | Reverse a list. Non-sloppy version, but copied from Prelude. Shame on me!
reverseLinkedList :: List a -> List a
reverseLinkedList l = rev l Nil
    where
        rev Nil           a = a
        rev (Cons x rest) a = rev rest (Cons x a)

-- | Reverse a list, sloppy version
reverseLinkedList' :: List a -> List a
reverseLinkedList' = fromList . reverse . toList

