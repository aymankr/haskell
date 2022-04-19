{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib
    ( prefixes
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

-- 4 -- path x to y exists in the graph
hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] a b = False
hasPath [x] a b = False


------ HIGHER ORDER FUNCTIONS -------
app :: (a -> b) -> a -> b
app f x = f x

add1 :: Int -> Int
add1 x = x + 1

--- anonnymous fc ---
anonnymousFc = (\x y z -> x + y + z)

-- MAP --
mappedList = map (\x -> x + 5) [1,2,3] -- -> [6,7,8]
mappedList2 = map (\(x,y) -> x+y) [(1,1),(2,2),(3,3)] -- -> [2,4,6]

-- FILTER --
filteredList = filter (\x -> even x) [1,2,3,4] -- -> [1,3]
filteredList2 = filter (\(x,y) -> x < y) [(1,3),(3,5), (9,3), (5,1)] -- -> [(9,3), (5,1)]

----- PARTIAL FUNCTIONS APPLICATION AND CURRYING ------
--curr1 :: a -> b -> c -> d
--curr2 :: a -> (b -> (c -> d)) -- (....) is a function

-- examples --
--add'' :: Int -> Int -> Int
--add'' x y = x + y
--add'' x = (\y -> x + y)
--add'' = (\x -> (\y -> x + y))

-- returns : (\y -> 1 + y)

---- FUNCTION COMPOSITION ----
-- f . g  équivalent to (\x -> f (g x))

--descSort = reverse . sort
--descSort2 = (\x -> reverse (sort x))
--descSort3 x = reverse (sort x) -- all eq --

-- map --
map2D :: (a -> b) -> [[a]] -> [[b]]
map2D = map . map

--- $ Sign ---

signFc xs = map (\x -> x +1) (filter (\x -> x > 1) xs)
signFc xs = map (\x -> x +1) $ filter (\x -> x > 1) xs

------- FOLDING --------
sum' xs = foldl (\acc x -> acc + x) 0 xs

max' :: [a] -> a
max' = foldr1 (\x acc -> if (x > acc) then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc ) []

product' :: [a] -> a
product' = foldr1 (*)

filter' :: [a] -> [a]
filter' = foldr (\x acc -> if (even x) then x : acc else acc) []

filter2' :: (a -> Bool) -> [a] -> [a]
filter2' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x) -------------- _ and order --------------

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- datatypes --
-- data name = Constructor1 <args> | Constructor2 <args>...


data Color = 
    Red | Green | Blue

data Calculation = 
    Add Int Int | Sub Int Int | Mul Int Int | Div Int Int 
calc :: Calculation -> Int
calc (Add x y) = x+y
calc (Sub x y) = x-y
calc (Mul x y) = x*y
calc (Div x y) = div x y

data Tree a = Leaf | Node (Tree a) a (Tree a)
tree :: Tree Int
tree =  Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 Leaf)

----- ex ------
-- 1 -- all prefixes of a given list

prefixes :: [a] -> [[a]]
prefixes = foldl (\x acc -> [x] : (map ((:)x) acc)) []

-- 2 -- lagrange

lagrange :: [(Float, Float)] -> Float -> Float
lagrange xs x =
    foldl (\(xj,yj) acc -> 
        acc + yj*(auxlagrange xs x xj)) 0 xs


auxlagrange :: [(Float, Float)] -> Float -> Float -> Float
auxlagrange xs x xj =
    foldl (\(xm, ym) acc -> 
        if (xj /= xm) then acc * (x-xm)/(xj-xm)
        else acc) 1 xs

-- 4 -- fold elements of a trie in a preorder traversel

data Trie a = Leaf a | Node a [Trie a]
foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie 