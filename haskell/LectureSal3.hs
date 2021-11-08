-- We can call that our 'a' is of type Ord and Num.
-- Accumulated paramters
splitUp :: (Ord a, Num a) => [a] -> ([a],[a])
splitUp ns = split ns [] []
    where split []     neg pos = (neg,pos)
          split (n:ns) neg pos = 
           | n < 0             = split ns (n:negs) pos
           | otherwise         = split ns negs     (n:pos)

-- Recursion
splitUp' []     = ([],[])
splitUp' [x:xs] = | x < 0 = (n:neg,pos)
                  | otherwise = (neg, n:pos)
        where (neg,pos) = splitUp' ns 