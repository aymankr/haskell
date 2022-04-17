{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib
    ( someFunc
    ) where
import Data.Bits (Bits(xor))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- DECLARATIVE --
--sum [] = 0
--sum (x:xs) = x + sum xs // sum of all elements in array
--or
--sum = foldr (+) 0

-- LAZY --
func arg =
    let x = True -- int x = True... --
        y = False
        z = True
    in
    if z then x else y -- if z return x...

-- FUNCTIONS (definition) --
name arg1 arg2 argn = True -- <expr>

-- example --
inrange :: Integer -> Integer -> Integer -> Bool
inrange min max x = x >= min && x <= max

-- let declarative --
in_range2 min max x =
    let in_low_bound = min <= x
        in_upper_bound = max >= x
    in
    in_low_bound && in_upper_bound

-- where --
in_range3 min max x = ilb && iub
    where
        ilb = min <= x
        iub = max >= x

-- RECURSION --
fac n =
    if n <= 1 then
        1
    else
        n * fac (n-1)

-- guards --
fac2 n
    | n <= 1    = 1
    | otherwise = n * fac2 (n-1)


-- accumulators
aux = True -- auxiliair fc --
fac3 n = aux n 1
    where
        aux n acc
            | n <= 1    = acc
            | otherwise = aux (n-1) (n*acc)

-- LISTS --

-- example -- 
asc :: Int -> Int -> [Int]
asc n m
    | n == m = [m]
    | m < n = []
    | otherwise = n : asc (n+1) m

-- head [1,2,3] -> 1
-- tail [1,2,3] -> [2,3]
-- init [1,2,3] -> [1,2]
-- null [1,2] -> False

listComprehension = [ 2*x | x <- [1,2,3], x > 1 ] -- => [4,6]
listComprehension2 = [ (x,y) | x <- [1,2,3], y <- ['a', 'b'] ]

-- LISTS PATTERNS --
sum2 :: [Int] -> Int
sum2 [] = 0
sum2 (x:xs) = x + sum2 xs

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs)
    | even x = x : evens xs
    | otherwise = evens xs


-- TUPLES --
--addTuples :: [(Int), (Int)] -> [Int]
--addTuples xs = [ x+y | (x,y) <- xs ]
--addTuples[(1,2), (2,3)]


-- EXERCISES --
-- 1 -- check if element is in the list

elem2 :: (Eq a) => a -> [a] -> Bool
elem2 e (x:xs) = x == e || elem2 e xs

-- corr --

elem2corr :: (Eq a) => a -> [a] -> Bool
elem2corr _ [] = False
elem2corr e (x:xs) = (e == x) || elem2corr e xs

-- 2 -- remove all duplicates from given list
nub :: (Eq a ) => [a] -> [a]
nub [] = []
nub (x:xs)
    | x `elem` xs = nub xs
    | otherwise = x : nub xs

-- 3 -- return true if list of ascending order
isAsc :: [Int] -> Bool
isAsc (x:xs)
    | x > head xs = False
    | otherwise = isAsc xs

-- corr --
isAscCorr :: [Int] -> Bool
isAscCorr [] = True
isAscCorr [x] = True
isAscCorr (x:y:xs)
    = (x <= y) && isAscCorr(y:xs)



