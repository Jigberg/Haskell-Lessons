import Test.QuickCheck
{-    
    Lecture Week 1, part A   David Sands, 2019 -}
    
-- Plan: FPFTW-- Exchange rate calculation--   concepts: simple data, types, testing, def by cases--             type inference vs checking-- exchangeRate

exchangeRate = 12.385  -- SEK per Brexit
-- functions
toSEK :: Double -> Double
toSEK sek    = sek * exchangeRate
toGBP pounds = pounds / exchangeRate
-- properties (testing)

prop_exchange sek = toSEK (toGBP sek) ~== sek
n ~== m = abs (n - m) < epsilon  
    where epsilon = 10e-10
    
abs' n  | n < 0     = -n      
        | otherwise = n
-- if n < 0 then -n else n
-- Random property testing using QuickCheck
-- import Test.QuickCheck
-- Definition by cases (guards)
-----------------------------------------------------------
-- Definition by recursion
-- power n k (computes n^k when k is a natural number)
-- power 2 3 = 2 * power 2 2
-- power 2 2 =     2 * 2
power :: Integer -> Integer -> Integer
power n k | k < 0  =     error "power of negative exponent"
power n 0  = 1
power n k  = n * power n (k-1)

prop_power n k = power n k'== n^k'   
    where k' = abs k

{- About Types   
Inspecting types in GHCi,   
browsing what's in scope
 -}
 
-- tuples, lists,
-- tuples, lists,

tuple1 :: (Integer, Bool, Double)
tuple1 = (42, True, 9.999)

f1 :: (Integer, Bool, Double) -> (Integer, Double)
f1 (n,b,d) = (power n 2, d/2)
 
string1 :: String
string1 = "Spam"
list1 :: [String]
list1 = ["Egg", "Chips", string1, string1, string1]
length' :: [t] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs
last' :: [a] -> a
last' []     = error "last on empty.."
last' [x]    = x
last' (_:xs) = last' xs
-- Functions over lists defined by recursion
-- length, last

-- List comprehensions
-- enums
ten = [1..10]

-- double every element in a list
double xs = [2 * x | x <- xs]
-- odd elements of a list
odds xs = [ x | x <- xs, odd x]
-- multiple generators
 
pairs :: [(Integer,Char)]
pairs = [(n,c) | n <- [1..4], c <- ['a'..'d'] ]
 
-- Quicksort
 
qsort [] = []
qsort (x:xs) = qsort lo ++ [x] ++ qsort hi  
    where   
    lo = [y | y <- xs, y < x]
    hi = [z | z <- xs, z >= x]

-----------