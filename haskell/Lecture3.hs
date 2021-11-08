import Test.QuickCheck

-- Data.List(sort) imports only the sort function.
import Data.List(sort)

-- We instead of automagiacly getting all preludes, imports them but leave out some.
import Prelude hiding ((++), reverse,drop,take)

-- We rename the "left out" as P instead. So to use ex. ++ we would write P.++
import qualified Prelude as P((++),reverse,drop,take)

-- our personal Append function
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x:(xs ++ ys)


-- reverse 
-- reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

reverse' xs = revInto [] xs  
    where revInto acc []     = acc        
          revInto acc (y:ys) = revInto (y:acc) ys

-- Take , takes x element from the start of the list. Take 2 [1,2,3,4,5] = [1,2]
-- Drop , takes x element from the start of the list and gives you what is left. Drop 2 [1,2,3,4,5] = [3,4,5]
-- If the Take and Drop exceeds the list Length it gives and leaves you what it can and does NOT give you syntax error. Neg numbers dont give syntax error either.

take n _ | n <= 0 = []
take _ []          = []
take n (x:xs)      = x : take (n-1) xs

drop n xs | n <= 0 = xs
drop _ []          = []
drop n (_:xs)      = drop (n-1) xs

-- prop_take :: Int -> [a] -> Property
prop_take n xs = n >= 0 ==> length (take n xs) <= n

--prop_takedrop :: Int -> [Int] -> Bool
prop_takedrop n xs = (take n xs ++ drop n xs) == xs  where types = xs :: [Int]

-- connecting take and drop?
-- simple property of take (==>, classify)
-- quickCheck types; verboseCheck, classify, collect

-- elem (revisiting list comprehensions)
-- [not covered this year]-- zip, unzip 