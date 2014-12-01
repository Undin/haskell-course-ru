{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
module ITMOPredule.Algebra where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
-- всевозможные инстансы для классов ниже 

-- Если не страшно, то реализуйте их и для
import ITMOPrelude.List
import ITMOPrelude.Tree

-- Классы
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

class Monoid a => Group a where
    ginv :: a -> a

-- Инстансы писать сюда

newtype Sum a = Sum { getSum :: a } 
newtype Product a = Product { getProduct :: a }
newtype Any a = Any { getAny :: a}
newtype All a = All { getAll :: a}

instance Monoid (Sum Nat) where
	mempty = Sum natZero
	mappend a b = Sum $ getSum a +. getSum b

instance Monoid (Sum Int) where
	mempty = Sum intZero
	mappend a b = Sum $ getSum a .+. getSum b

instance Monoid (Sum Rat) where
	mempty = Sum $ Rat intZero natOne
	mappend a b = Sum $ getSum a %+ getSum b

instance Monoid (Product Nat) where
	mempty = Product natOne
	mappend a b = Product $ getProduct a *. getProduct b

instance Monoid (Product Int) where
	mempty = Product intOne
	mappend a b = Product $ getProduct a .*. getProduct b

instance Monoid (Product Rat) where
	mempty = Product $ Rat intOne natOne
	mappend a b = Product $ getProduct a %* getProduct b 

instance Monoid (Any Bool) where
	mempty = Any False
	mappend (Any x) (Any y) = Any (x || y)

instance Monoid (All Bool) where
	mempty = All True
	mappend (All x) (All y) = All (x && y)

instance Monoid Tri where
	mempty = EQ
	mappend LT _ = LT
	mappend EQ x = x
	mappend GT _ = GT

instance Monoid (List a) where
	mempty = Nil
	mappend = (++)

instance Group (Sum Int) where
	ginv a = Sum $ intNeg $ getSum a

instance Group (Sum Rat) where
	ginv a = Sum $ ratNeg $ getSum a

instance Group (Product Rat) where
 	ginv a = Product $ ratInv $ getProduct a
