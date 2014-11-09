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

data Add a = Add a 
data Mul a = Mul a

instance Monoid (Add Nat) where
	mempty = Add natZero
	mappend (Add a) (Add b) = Add $ a +. b

instance Monoid (Add Int) where
	mempty = Add intZero
	mappend (Add a) (Add b) = Add $ a .+. b

instance Monoid (Add Rat) where
	mempty = Add $ Rat intZero natOne
	mappend (Add a) (Add b) = Add $ a %+ b

instance Monoid (Mul Nat) where
	mempty = Mul natOne
	mappend (Mul a) (Mul b) = Mul $ a *. b

instance Monoid (Mul Int) where
	mempty = Mul intOne
	mappend (Mul a) (Mul b) = Mul $ a .*. b

instance Monoid (Mul Rat) where
	mempty = Mul $ Rat intOne natOne
	mappend (Mul a) (Mul b) = Mul $ a %* b 

instance Monoid (List a) where
	mempty = Nil
	mappend = (++)

instance Group (Add Int) where
	ginv (Add a) = Add $ intNeg a

instance Group (Add Rat) where
	ginv (Add a) = Add $ ratNeg a

instance Group (Mul Rat) where
 	ginv (Mul a) = Mul $ ratInv a
