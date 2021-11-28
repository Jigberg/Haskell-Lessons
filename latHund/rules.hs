-- The left arrow works similar to = but has some important differences.
-- Instead of binding for example a variable to a function, it binds the result. This becomes important when dealing with IO.
-- readFile or getLine delivers IO String, using <- allows us the String. 
x <- getLine
:t x == String

x = getLine
:t x >> IO String

-- Same can be done with functions. x = func and x <- func are NOT the same.



-- Do expressions
f int -> int
f x = do


-- List comprehensions
-- Split with a guard, the right side of the guard is for an expression changing/modifying the left side. 
[toUpper c | c <- s]




-- Syntax in Functions    Pattern Matching   Guards   Let it Be   Case expressions

-- pattern matching is one way to construct a function in haskell. We call this   Syntax in Functions
-- Pattern matching is basic to start. Give cases for all the different parameters the function can get.
-- It goes top to bottom and will stop once it has done 1. Def will never be called for 7 and everything other then 7 will call Def.

lucky :: (Integral a) => a -> String  
lucky 7 = putStrLn "Vic"  
lucky x = putStrLn "Def" 

-- Guards is similar to a if statement. Guards are generally not as "strict" as Pattern and can be used more freely, speaking in a loose coding terms.
-- Like Pattern does the function always return something therefore we have otherwise, "else". It also stops when an argument is true.
-- if x = 3 then äpple would be typed and only äpple. Even tough the others are true as well. 
lucky :: (Integral a) => a -> String 
lucky x
 | x < 5     = putStrLn "Äpple"
 | x < 10    = putStrLn "pärron"
 | x < 15    = putStrLn "Apelsin"
 | otherwise = "Pepparkaka"