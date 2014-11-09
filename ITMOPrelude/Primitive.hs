{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read,error)

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show, Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp Zero     Zero     = EQ
natCmp Zero     (Succ _) = LT
natCmp (Succ _) Zero     = GT
natCmp (Succ n) (Succ m) = natCmp n m

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
Zero -. m = Zero
n -. Zero = n
(Succ n) -. (Succ m) = n -. m

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod _ Zero = error "Division by Zero"
natDivMod n m = if' (natLt n m) (Pair Zero n) (Pair (natOne +. fst p) (snd p))
	where p = natDivMod (n -. m) m

natDiv n = fst . natDivMod n -- Целое
natMod n = snd . natDivMod n -- Остаток

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd n Zero = n
gcd n m = if' (natLt n m) (gcd m n) (gcd m (natMod n m))   

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = IntZero | Pos Nat | Neg Nat deriving (Show,Read)

intZero   = IntZero   -- 0
intOne    = Pos Zero     -- 1
intNegOne = Neg Zero -- -1

-- n -> - n
intNeg :: Int -> Int
intNeg IntZero = IntZero
intNeg (Neg n) = Pos n
intNeg (Pos n) = Neg n

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp IntZero IntZero = EQ
intCmp (Pos n) (Pos m) = natCmp n m
intCmp (Neg n) (Neg m) = natCmp m n
intCmp (Neg _) IntZero = LT
intCmp (Neg _) (Pos _) = LT
intCmp IntZero (Pos _) = LT
intCmp _ _ = GT

intEq :: Int -> Int -> Bool
intEq IntZero IntZero = True
intEq (Pos n) (Pos m) = natEq n m
intEq (Neg n) (Neg m) = natEq n m
intEq _ _ = False

intLt :: Int -> Int -> Bool
intLt IntZero (Pos _) = True
intLt (Neg _) IntZero = True
intLt (Neg _) (Pos _) = True
intLt (Pos n) (Pos m) = natLt n m
intLt (Neg n) (Neg m) = natLt m n
intLt _ _ = False

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
IntZero .+. m = m
(Pos n) .+. (Pos m) = Pos (n +. m +. natOne)
(Neg n) .+. (Neg m) = Neg (n +. m +. natOne)
(Pos n) .+. (Neg m) = 
	case x of
		EQ -> IntZero
		LT -> Neg (m -. n -. natOne)
		GT -> Pos (n -. m -. natOne)
	where
		x = natCmp n m
n .+. m = m .+. n

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
IntZero .*. _ = IntZero
(Pos n) .*. (Pos m) = Pos ((n +. natOne) *. (m +. natOne) -. natOne)
(Pos n) .*. (Neg m) = Neg ((n +. natOne) *. (m +. natOne) -. natOne)
(Neg n) .*. (Neg m) = Pos ((n +. natOne) *. (m +. natOne) -. natOne)
n .*. m = m .*. n

-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat deriving (Show, Read)

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat IntZero _) = error "Division by zero"
ratInv (Rat (Pos x) y) = Rat (Pos $ y -. natOne) (x +. natOne)
ratInv (Rat (Neg x) y) = Rat (Neg $ y -. natOne) (x +. natOne)

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat x1 y1) (Rat x2 y2) = intCmp (x1 .*. (Pos $ y2 -. natOne)) (x2 .*. (Pos $ y1 -. natOne))

ratEq :: Rat -> Rat -> Bool
ratEq (Rat x1 y1) (Rat x2 y2) = intEq (x1 .*. (Pos $ y2 -. natOne)) (x2 .*. (Pos $ y1 -. natOne))

ratLt :: Rat -> Rat -> Bool
ratLt (Rat x1 y1) (Rat x2 y2) = intLt (x1 .*. (Pos $ y2 -. natOne)) (x2 .*. (Pos $ y1 -. natOne))

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat x1 y1) %+ (Rat x2 y2) = Rat ((x1 .*. (Pos $ y2 -. natOne)) .+. (x2 .*. (Pos $ y1 -. natOne))) (y1 *. y2)

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat x1 y1) %* (Rat x2 y2) = Rat (x1 .*. x2) (y1 *. y2)

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b
