module Tenta where
import Test.QuickCheck
import Data.List
import Data.Char
import Data.Maybe





data Name = CanBeX | OrBeY | EvenZ
-- notice capital letters

instance Show Name where
    show CanBeX = "x"
    show OrBeY = "y"
    show EvenZ = "z"

-- A way of deriving for example "Show" more precise. 


allProps :: [a -> Bool] -> a -> Bool
allProps [] prop = True
allProps (x:xs) prop = x prop && allProps xs prop

allProps' :: [a -> Bool] -> a -> Bool
allProps' list prop = and [ p prop | p <- list]

dimension :: [[a]] -> Maybe Int
dimension xss
 | all (== (length xss)) (map length xss) = (Just (length xss))
 | otherwise = (Nothing)


data IntSquare = Square [[Int]]
  deriving (Eq,Show)

prop_Square (Square s) = isJust (dimension s)

instance Arbitrary IntSquare where
  arbitrary = do dim <- abs <$> arbitrary
                 sq  <- vectorOf dim (vectorOf dim arbitrary)
                 return $ Square sq 


diag :: [[a]] -> [a]
diag xss = diagHelper (length xss)
   where diagHelper 0 = []
         diagHelper m = [((xss!!(m-1))!!(m-1))] ++ diagHelper (m-1)
