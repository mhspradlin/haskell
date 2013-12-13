import Data.Monoid
import Data.Foldable
import Control.Applicative
import Control.Monad

data Tree a = Vacancy | Leaf a | Node (Tree a) (Tree a)
	deriving (Show)

instance Monoid (Tree a) where
	mempty = Vacancy
	mappend Vacancy i = i
	mappend i Vacancy = i
	mappend (Leaf i) (Leaf j) = Node (Leaf i) (Leaf j)
	mappend (Leaf i) (Node j k) = Node (j <> (Leaf i)) (k)
	mappend (Node h i) (Leaf k) = Node h (Leaf k <> i)
	mappend (Node h i) (Node j k) = Node (h <> j) (i <> k)

instance Functor Tree where
	fmap f Vacancy = Vacancy
	fmap f (Leaf i) = Leaf (f i)
	fmap f (Node a b) = Node (fmap f a) (fmap f b)

instance Foldable Tree where
	foldMap f Vacancy = mempty
	foldMap f (Leaf i) = f i
	foldMap f (Node a b) = (foldMap f a) <> (foldMap f b)
	
instance Monad Tree where
	return x = Leaf x
	Vacancy >>= f = Vacancy
	Leaf x >>= f = f x
	Node a b >>= f = Node (a >>= f) (b >>= f)

instance Applicative Tree where
	pure x = Leaf x
	(Leaf f) <*> (Node x y) = Node (Leaf f <*> x) (Leaf f <*> y)
	(Leaf f) <*> (Leaf x) = Leaf (f x)
	(Node f g) <*> (Node x y) = Node (f <*> x) (g <*> y)
	_ <*> _ = Vacancy  

instance MonadPlus Tree where
	mzero = Vacancy
	Vacancy `mplus` i = i
	(Node a b) `mplus` (Node h i) = Node (a `mplus` h) (b `mplus` i)
	(Leaf a) `mplus` i = Node (Leaf a) i
	i `mplus` j = i
