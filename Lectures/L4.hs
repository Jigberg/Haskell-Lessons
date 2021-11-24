module L02B where
import Test.QuickCheck
import Data.Char(isSpace)

-- Examples from Week 2, lecture B
-- Higher-Order Functions
-- dave 2019-11-14
------------------------------------------
-- The most basic HOFs (seen in the kahoot)
-- 1. map, examples, definition
-- 2. filter
-- map, filter def with list comprehensions

map'    f xs = [ f x | x <- xs     ]

filter' p xs = [   x | x <- xs, p x]

-- Example of s common programming pattern: 
-- combining the elements of a list
-- (e.g. sum, product, and, or)

sum' []     =  0 
sum' (n:ns) = n + sum' ns

prod [] = 1
prod (n:ns) = n * prod ns

concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

and' [] = True
and' (b:bs) = b && and' bs

foldr' op b []     = b
foldr' op b (x:xs) = x `op` foldr' op b xs

and'' bs = foldr (&&) True bs
prod' ns = foldr (*)  1    ns



-- foldr op base (a:b:c:...) == a `op` (b `op` (... `op` base)...)

-- Q: define (++) using foldr


-- foldl op z (a:b:c:...) == ((base `op` a) `op` b) ...)
-- An accumulating parameter version of fold

-- Exercises:
-- Define (++) using foldr, reverse using foldl (and foldr inefficient)
--

verse :: String
verse = unlines
 ["Livet är härligt"
 ,"Tavaritj vårt liv är härligt"
 ,"Vi alla våra små bekymmer glömmer"
 ,"när vi har fått en tår på tand en skål"
 ]

-- How to "feed" HOFs?

-- (i) Define a suitable function,
-- feed its name to the HOF

unlines' ss = foldr (\s1 s2 -> s1 ++ "\n" ++ s2) "" ss

--   where join s1 s2 = s1 ++ "\n" ++ s2
  

-- Example use of foldr: unlines (c.f. lines)

-- HOFs for generalising common computation patterns 
-- Example: takeLine, takeWord
takeLine [] = []
takeLine (c:cs)
    | c /= '\n' = c : takeLine cs
    | otherwise = []
    
prop_takeLine = takeLine verse == "Livet är härligt"
prop_takeWord = takeWord verse == "Livet"

takeWord [] = []
takeWord (c:cs)
    | not(isSpace c)  = c : takeWord cs
    | otherwise       = []

takeWhile' p [] = []
takeWhile' p (c:cs)
    | p c        = c : takeWhile' p cs
    | otherwise  = []
    
-- Defining takeLine using takeWhile, illustrating
takeLine' s = takeWhile (/= '\n') s
takeWord' s = takeWhile (\c -> not (isSpace c)) s


 

-- (ii) anonymous functions 
-- lambda expressions

-- (iii) sections

-- limitations: e.g. double and add one...


-- [Another pattern: segments (e.g. words, lines, commaSep)
-- one example: commSep (introducing dropWhile and span)

eg = "one,two,three"
commSep [] = []
commSep cs = first : commSep (drop 1 rest)
 where
    (first, rest) = span (/= ',') cs
--  first = takeWhile (/= ',') cs
--  rest  = dropWhile (/= ',') cs


--    (see video for full example) ]


-------------------------------------
-- More ways to build functions: Partial application
-- [slides]

-- consider take :: Int -> [t] -> [t]
-- "Given an Int and a list of t
--  and creates a list of t"

-- Eta reduction
-- f x1 ... xn = e xn  can be rewritten...

takeTwo xs = take 2 xs

-- use hlint <filename> to find this kind of thing (C-c C-v)

-- Aside: 
-- Fixity.  How do you know when to skip brackets?
-- $ 
 
-- Example Design [Slides]
-- countWords = undefined

-- [Skip] Test.QuickCheck.Functions for testing HOFs  (Fun _ f)
--
-- import Test.QuickCheck.Functions
-- prop_takeWhile (Fun _ p) xs = takeWhile p xs ++ dropWhile p xs == xs


