{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonadJoin where
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Primitive
-- Эти
import ITMOPrelude.Categories hiding ((.))
import ITMOPrelude.Categories.MonadFish

-- делаем из нас

-- делаем из нас
instance MonadJoin m => Monad m where
    return = returnJoin
    x >>= f = join $ fmap f x

instance MonadJoin m => MonadFish m where
    returnFish = returnJoin
    f >=> g = \x -> join $ fmap g (f x)