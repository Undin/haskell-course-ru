{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree
-- всевозможные инстансы для классов ниже

--------------------------------------------------------------------------------
-- Классы
class Category cat where
    id  :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

(>>) :: Monad m => m a -> m b -> m b
ma >> mb = ma >>= (\_ -> mb)

--------------------------------------------------------------------------------
-- Инстансы писать сюда
instance Functor Maybe where
	fmap _ Nothing = Nothing
	fmap f (Just a) = Just $ f a

instance Functor (Either a) where
	fmap _ (Left x) = Left x
	fmap f (Right y) = Right $ f y

instance Functor List where
	fmap _ Nil = Nil
	fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Functor Tree where
	fmap _ Leaf = Leaf
	fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Monad Maybe where
	return = Just
	Nothing >>= _ = Nothing
	(Just a) >>= f = f a

instance Monad (Either a) where
	return = Right
	(Left x) >>= _ = Left x
	(Right y) >>= f = f y

instance Monad List where
	return a = Cons a Nil
	Nil >>= _ = Nil
	l  >>= f = concat $ map f l


--------------------------------------------------------------------------------
-- Монада State

newtype State s a = State { runState :: s -> (s, a) }

instance Monad (State s) where
    return x = State $ \s -> (s, x)
    s >>= f = State $ \st0 -> let (st1, a) = runState s st0 in runState (f a) st1
