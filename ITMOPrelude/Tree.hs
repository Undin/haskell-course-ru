module ITMOPrelude.Tree where

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

emptyTree :: Tree a
emptyTree = Leaf

find :: (Ord a) => Tree a -> a -> Bool
find Leaf _ = False
find (Node x l r) a
	| a == x = True
	| a < x = find l a
	| a > x = find r a

treeMin :: (Ord a) => Tree a -> a
treeMin Leaf = error "empty tree"
treeMin (Node x Leaf _) = x
treeMin (Node _ l _) = treeMin l

treeMax :: (Ord a) => Tree a -> a
treeMax Leaf = error "empty tree"
treeMax (Node x _ Leaf) = x
treeMax (Node _ _ r) = treeMax r

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf a = Node a Leaf Leaf
insert t@(Node x l r) a
	| a == x = t
	| a < x = Node x (insert l a) r
	| a > x = Node x l $ insert r a


erase :: (Ord a) => Tree a -> a -> Tree a
erase Leaf _ = Leaf
erase (Node x l r) a
	| a == x = case l of
		Leaf -> r
		Node _ _ _ -> let m = treeMax l in Node m (erase l m) r
	| a < x = Node x (erase l a) r
	| a > x = Node x l $ erase r a


treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Leaf = Leaf
treeMap f (Node x l r) = Node (f x) (treeMap f l) (treeMap f r)

foldtLHR :: (b -> a -> b) -> b -> Tree a -> b
foldtLHR _ acc Leaf = acc
foldtLHR f acc (Node x l r) = foldtLHR f (f (foldtLHR f acc l) x) r
