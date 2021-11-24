-- Лабораторна робота №3
-- студент групи АнД-31
-- Українець Андрій Олександрович
-- Варіант №12

-- Мета роботи: Набути досвiду визначення та використання функцiй вищого порядку.

-- Завдання 1. Роздiлити список на двi частини при заданiй довжинi першої n, напр. при n=3: "abcdefghik"⇒ ("abc "defghik").
split :: Int -> [Char] -> ([Char],[Char])
split 1 [] = ([],[])
split 1 (x:xs) = ([x],xs)
split n (x:xs) = (x:fst(split(n-1) xs),snd(split(n-1) xs))

-- Результат роботи:
-- Для тестування потрібно ввести так команди:
-- ghci
-- :load Lab3\\Lab3.hs
-- split 6 "qwertyuio"
-- ("qwertyu","io")

-- Завдання 2. Перевiрити гiпотезу Ґольдбаха у вказаному дiапазонi.
dm :: Int -> [ Int ] -> [ Int ]
dm x xs = [ y | y <- xs , y `mod ` x /= 0]

da :: [ Int ] -> [ Int ]
da ( x : xs ) = x : da ( dm x xs )

primes :: [ Int ]
primes = da [2 ..]

goldbach :: Int -> [(Int, Int)]
goldbach n = [(a, b) | a <- takeWhile (<n) primes, b <- takeWhile (<n) primes, n == a + b]

-- Результат роботи:
-- Для тестування потрібно ввести такі команди:
-- goldbach 8
-- [(3,5),(5,3)]

-- Висновок. У ході виконання лабораторної роботи №3 було розроблено ф-ю, яка розділяє масив на дві частини, а також ф-ю,
-- яка перевіряє гіпотезу Ґольдбаха. Головне це ми навчилися використовувавти ф-ї вищого порядку.