-- Лабораторна робота №5
-- студент групи АнД-31
-- Українець Андрій Олександрович
-- Варіант №12

-- Мета роботи: Ознайомитись з модульною органiзацiєю програм та засобами введення-виведення. Набути досвiду компiляцiї Haskell-програм.

--  Роздiлити список на двi частини при заданiй довжинi першої n, напр. при n=3: "abcdefghik"⇒ ("abc" "defghik").
split :: Int -> [Char] -> ([Char],[Char])
split 1 [] = ([],[])
split 1 (x:xs) = ([x],xs)
split n (x:xs) = (x:fst(split(n-1) xs),snd(split(n-1) xs))


-- Завдання 1юРеалiзувати та скомпiлювати одну з програм, розроблених у лабораторнiй роботi №3 для Вашого варiанта з введенням даних:
-- а) з клавiатури, б) з файлу та виведенням результатiв: в) на екран, г) у файл.

fromKeyboardToConsole = do
    putStrLn "Enter size first list:"
    inputN <- getLine
    putStrLn "Enter list:"
    list <- getLine
    let n = read inputN :: Int
    let answer = split n list
    print answer

-- Результат роботи:
-- Для тестування потрібно ввести такі команди:
-- ghci
-- :l Lab5\\Lab5.hs
-- fromKeyboardToConsole
-- Enter size first list:
-- 3
-- Enter list:
-- qwertyu
-- ("qwe","rtyu")

fromKeyboardToFile = do
    putStrLn "Enter size first list:"
    inputN <- getLine
    putStrLn "Enter list:"
    list <- getLine
    let n = read inputN :: Int
    let answer = split n list
    writeFile "Lab5\\fromKeyboardToFile.txt" (show answer)

-- Результат роботи:
-- Для тестування потрібно ввести такі команди:
-- fromKeyboardToFile
-- Enter size first list:
-- 5
-- Enter list:
-- qwertyui
-- Переглянути файл "fromKeyboardToFile.txt"
-- ("qwert","yui")

fromFileToConsole = do
        list <- readFile "Lab5\\input.txt"
        let answer = split 4 list
        print answer

-- Результат роботи:
-- Для тестування потрібно ввести такі команди:
-- fromFileToConsole
-- Дані беруться з файлу "input.txt"
-- ("qwer","tyu")

fromFileToFile = do
    list <- readFile "Lab5\\input.txt"
    let answer = split 2 list
    writeFile "Lab5\\fromFileToFile.txt" (show answer)

-- Результат роботи:
-- Для тестування потрібно ввести такі команди:
-- fromFileToFile
-- Дані беруться з файлу "input.txt"
-- Переглянути файл "fromFileToFile.txt"
-- ("qw","ertyu")

-- Висновок. У ході виконання лабораторної роботи №4 було взято одне з завдань третьої лабораторної роботи і розроблено для нього такі ф-ї.
-- Введення з клавіатури та файлу, а також вивід у консоль і у файл.