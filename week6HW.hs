
-- Part 1 --

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n -2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Part 2 --

fibs2 :: [Integer]
fibs2 = fibs2' 0 1

fibs2' :: Integer -> Integer -> [Integer]
fibs2' x y = res : fibs2' y res
    where res = x + y

-- Part 3 --

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a stream) = a : streamToList stream

instance Show a => Show (Stream a) where
    show = foldr (\a b -> a ++ " " ++ b) "..." .
                  map show . take 10 . streamToList

-- Part 4 --

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)