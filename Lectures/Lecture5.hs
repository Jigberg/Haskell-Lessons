module Lecture5 where
import Data.List
import Data.Char

copyFile :: FilePath -> FilePath -> IO()
copyFile fromFile toFile = do c <- readFile fromFile
                              writeFile toFile c

-- do  needs to be split like above.
-- Everything in the (do) block should have the same type.

-- Find the longest word in file
longest :: IO String
longest = do
          wlist <- readFile "path"
          return (long wlist)

    where long :: String -> String
          long = snd . maximum . map (\w -> (length w, w)) . words

-- words breaks a string into a word list.
-- snd (second) takes the second element of a pair or tuple.
-- maximum finds the biggest type in a order. Numbers, letters etc.

dotwice :: IO a -> IO (a,a)
dotwice i = do 
            a1 <- i
            a2 <- i
            return (a1,a2)

dont :: IO a -> IO ()
dont i = return ()

test :: IO Int
test = do 
       ans <- return 42
       return 5

-- return doesnt return a thing like in java or python. 

mySequence_ :: [IO a] -> IO ()
mySequence_ []     = return ()
mySequence_ (i:is) = do
                     i
                     mySequence_ is

mySequence :: [IO a] -> I0 [a]
mySequence [] = return []
mySequence (i:is) = do
                    a <- i
                    as <- mySequence is
                    return (a:as)
