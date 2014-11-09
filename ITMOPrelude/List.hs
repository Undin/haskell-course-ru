{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length Nil = Zero
length (Cons _ y) = Succ (length y)

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ l = l
(Cons a t) ++ l = Cons a (t ++ l)  

-- Список без первого элемента
tail :: List a -> List a
tail Nil = error "list must be not empty"
tail (Cons _ l) = l

-- Список без последнего элемента
init :: List a -> List a
init Nil =  error "list must be not empty"
init (Cons _ Nil) = Nil
init (Cons a l) = Cons a (init l)

-- Первый элемент
head :: List a -> a
head Nil =  error "list must be not empty"
head (Cons a _) = a

-- Последний элемент
last :: List a -> a
last Nil =  error "list must be not empty"
last (Cons a Nil) = a
last (Cons _ l) = last l

-- n первых элементов списка
take :: Nat -> List a -> List a
take _ Nil = Nil
take Zero _ = Nil
take (Succ x) (Cons a l) = Cons a (take x l)

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop _ Nil = Nil
drop Zero l = l
drop (Succ x) (Cons _ l) = drop x l

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter _ Nil = Nil
filter p (Cons a l) = if' (p a) (Cons a $ filter p l) (filter p l)

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter _ Nil = Nil
gfilter p (Cons a l) = 
	case (p a) of 
		Nothing -> gfilter p l
		Just b -> Cons b $ gfilter p l


-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile p (Cons a l) = if' (p a) (Cons a $ takeWhile p l) Nil

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile p x@(Cons a l) = if' (p a) (dropWhile p l) x

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
-- span p = undefined
span _ Nil = Pair Nil Nil
span p (Cons a l) = if' (p a) (Pair (Cons a x) y) (Pair Nil l)
	where
		Pair x y = span p l

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p l = span (not . p) l

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons a _)  !! Zero = a
(Cons _ l) !! (Succ x) = l !! x

-- Список задом на перёд
reverse :: List a -> List a
reverse l = snd $ f (Pair l Nil)
	where
		f p@(Pair Nil _) = p 
		f (Pair (Cons x l) a) = f $ Pair l $ Cons x a  

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) = let p = (subsequences xs) in p ++ (map (Cons x) p)


-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations = undefined

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat a = Cons a $ repeat a

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl _ acc Nil = acc
foldl f acc (Cons x xs) = foldl f (f acc x) xs

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl _ acc Nil = Cons acc Nil
scanl f acc (Cons x xs) = Cons acc $ scanl f (f acc x) xs  

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr _ acc Nil = acc
foldr f acc (Cons x xs) = f x $ foldr f acc xs

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr _ acc Nil = Cons acc Nil
scanr f acc (Cons x xs) = let l = scanr f acc xs in Cons (f x $ head l) l 

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons a l) = Cons (f a) $ map f l

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons x xs) = x ++ concat xs

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap _ Nil = Nil
concatMap f (Cons x xs) = f x ++ concatMap f xs

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (Cons x xs) (Cons y ys) = Cons (Pair x y) $ zip xs ys

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith _ Nil _ = Nil
zipWith _ _ Nil = Nil
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) $ zipWith f xs ys
