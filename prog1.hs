import Control.Applicative

data List a = List a (List a) | Nil deriving (Show, Eq)

instance Functor List where
	fmap f (List d l) = List (f d) (fmap f l)
	fmap _ Nil = Nil

instance Applicative List where
	pure x = List x Nil
	Nil <*> _ = Nil
	_ <*> Nil = Nil
	i1@(List f _) <*> (List d l) = List (f d) (i1 <*> l)

instance Monad List where
	return x = List x Nil
	(List x Nil) >>= f = f x
	fail _ = Nil

createList :: a -> List a
createList x = List x Nil


insertHead :: (Num a, Eq a) => List a -> a -> List a
insertHead Nil x = List x Nil
insertHead list@(List d l) x = List x (list)

insertTail:: (Num a, Eq a) => List a -> a -> List a
insertTail Nil x = List x Nil
insertTail (List d l) x = List d (insertTail l x)

getHead :: (Num a, Eq a) => List a -> a
getHead (List d l) = d
getHead Nil = error "empty list"

getTail :: (Num a, Eq a) => List a -> List a
getTail Nil = Nil
getTail (List d l) = l

isEmpty :: (Num a, Eq a) => List a -> Bool
isEmpty Nil = True
isEmpty _ = False

delete:: (Num a, Eq a) => List a -> a -> List a
delete Nil _ = Nil
delete (List d l) x = if d == x then Nil else (List d (delete l x))