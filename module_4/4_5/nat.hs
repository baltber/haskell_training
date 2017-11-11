module Nat where

data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat x = Suc (toNat (x -1))

add :: Nat -> Nat -> Nat
add = \x y -> toNat $ fromNat x + fromNat y

mul :: Nat -> Nat -> Nat
mul = \x y -> toNat $ fromNat x * fromNat y

fac :: Nat -> Nat
fac = \x -> toNat $ product[1..(fromNat x)]


add' :: Nat -> Nat -> Nat
add' Zero x = x
add' (Suc x) y = add x (Suc y) 

mul' :: Nat -> Nat -> Nat
mul' Zero _ = Zero
mul' (Suc x) y = add (mul x y) y

fac' :: Nat -> Nat
fac' Zero = Suc Zero
fac' n1@(Suc n) = mul n1 (fac n)
