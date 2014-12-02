{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonadFish where
import ITMOPrelude.Categories.MonadFish
import ITMOPrelude.Primitive
-- Эти
import ITMOPrelude.Categories hiding ((.), id)
import ITMOPrelude.Categories.MonadJoin

-- делаем из нас
instance MonadFish m => Monad m where
    return = returnFish
    x >>= f = (id >=> f) x

instance MonadFish m => Functor m where
    fmap f x = x >>= (return . f)

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish	
    join = id >=> id
