--Реализуйте функцию seqA, находящую элементы следующей рекуррентной последовательности

--a0=1;a1=2;a2=3;ak+3=ak+2+ak+1−2ak.
--a0=1;a1=2;a2=3;ak+3=ak+2+ak+1−2ak.
--Попытайтесь найти эффективное решение.

--GHCi> seqA 301
--1276538859311178639666612897162414

seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = let
	   rec a b c 0 = a
	   rec a b c n = rec b c (c + b -2*a)(n-1)
	in rec 1 2 3 n

--Реализуйте функцию, находящую сумму и количество цифр десятичной записи заданного целого числа.

--sum'n'count :: Integer -> (Integer, Integer)
--sum'n'count x = undefined
--GHCi> sum'n'count (-39)
--(12,2)


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (sumd x', count x') where
    x' = abs x

    count :: Integer -> Integer
    count x = length' $ show x

    sumd :: Integer -> Integer
    sumd 0 = 0
    sumd x = (mod x 10) + sumd (div x 10)

    length' :: [a] -> Integer
    length' [] = 0
    length' (x:xs) = 1 + length' xs
