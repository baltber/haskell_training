module Sign where

--Напишите реализацию функции sign, которая возвращает 1, если ей передано положительное число,(-1), если отрицательное, и 0 в случае, когда передан 0.

sign :: Integer -> Integer
sign x = if x > 0 then 1 else if x < 0 then (-1) else 0
