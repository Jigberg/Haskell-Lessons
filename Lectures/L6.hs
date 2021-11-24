module L04A2 where
-- Lecture week 4A part 2
-- Symbolic expressions
-- dave@chalmers.se 
-- 2019-11-25
import Test.QuickCheck
import Data.List(union)
import Data.Maybe(fromJust) -- not used
------------------------------------------------------
data Expr
  = Num Integer
  | Var String    -- <<<<---- NEW!
  | Add Expr Expr
  | Mul Expr Expr
 deriving Eq

ex1 = Mul (Add (Var "y") (Num 2)) (Var "x") 
ex2 = Add (Var "x") (Mul (Num 2) (Var "y"))
ex3 = Num (-5) `Add` (Num 2 `Mul` Num 4)

showExpr (Num n) = show n
showExpr (Var s) = s
showExpr (Add e e') = 
  showExpr e ++ " + " ++ showExpr e'
showExpr (Mul e e') = 
  showFactor e ++ " * " ++ showFactor e'
  where 
    showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
    showFactor e           = showExpr e

instance Show Expr
   where show = showExpr

vars :: Expr -> [String]
vars (Num _) = []
vars (Var s) = [s]
vars (Add e1 e2) = vars e1 `union` vars e2
vars (Mul e1 e2) = vars e1 `union` vars e2

type Name = String
type Table = [(Name,Integer)]

eval :: Table -> Expr -> Integer
eval t e = eval' e where
  eval' (Num n) = n
  eval' (Var x) = case lookup x t of
                       Just n -> n
                       Nothing -> error ("no value for " ++ x)
  eval' (Mul e1 e2) = eval' e1 * eval' e2
  eval' (Add e1 e2) = eval' e1 + eval' e2
 
extable  = [("x",2)]
extable' = [("x",2),("y",3),("z",4)]


------------------------------------------------------
-- generators for expressions

rExp :: Int -> Gen Expr
rExp s = frequency [(1,rNum), (1,rVar), (s, rBin)]
  where  
   range = 4

   rNum = Num <$> choose(-range,range)
   {- do 
       n <- choose(-range,range)
       return $ Num n            -}
   rVar = Var <$> elements ["x","y","z"]
   
   rBin = do
      op <- elements [Add,Mul]
      let s' = s `div` 2 
      e1 <- rExp s'
      e2 <- rExp s'
      return $ op e1 e2

instance Arbitrary Expr where
  arbitrary = sized rExp

-----------------------------------------------------------------------

derive :: String -> Expr -> Expr
-- derive x e 
-- the derivative of e with respect to x (d/dx) 
 
derive x (Add e1 e2) = add (derive x e1) (derive x e2)
derive x (Mul e1 e2) = add (mul (derive x e1) e2) (mul e1 (derive x e2))
derive x (Var y)
     | x == y        = Num 1
       
derive x _           = Num 0

add (Num n) (Num m) = Num (n + m)
add (Num 0) e       = e
add e       (Num 0) = e
add e1      e2      = Add e1 e2

mul (Num n) (Num m) = Num (n * m)
mul (Num 0) e       = Num 0
mul e       (Num 0) = Num 0
mul (Num 1) e       = e
mul e       (Num 1) = e
mul e1      e2      = Mul e1 e2



-- ***** simplify

data EnvExpr = EnvExpr Table Expr
   deriving Show

instance Arbitrary EnvExpr where
  arbitrary = do
        (l,m,n) <- arbitrary
        e       <- arbitrary
        return $ EnvExpr [("x", l), ("y", m), ("z", n)] e
        


prop_simplify (EnvExpr t e) = eval t (simplify e) == eval t e

simplify = undefined -- exercise


-- * end
