module Sudoku where

import Test.QuickCheck
import Data.List
import Data.Char(digitToInt)

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [ [Nothing | _ <- [1..9] ] | _ <- [1..9] ]

falseSudoku :: Sudoku
falseSudoku = Sudoku [ [Just 10 | _ <- [1..5] ] | _ <- [1..5] ]


-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku []) = False
isSudoku (Sudoku rows) = if length rows == 9
                          then checkRows (Sudoku rows)
                          else False

checkRows :: Sudoku -> Bool
checkRows (Sudoku [])     = True
checkRows (Sudoku (x:xs)) = check1Row x && length x == 9 
                            && checkRows (Sudoku xs)

check1Row :: Row -> Bool
check1Row []     = True
check1Row (x:xs) = checkCell x  && check1Row xs

checkCell :: Cell -> Bool
checkCell Nothing = True
checkCell (Just n) = n >= 1 && n <= 9

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku [] )    = True 
isFilled (Sudoku (x:xs)) = not (Nothing `elem` x) && isFilled (Sudoku xs)

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = putStr (stringSudoku sudoku)

stringSudoku :: Sudoku -> String
stringSudoku (Sudoku []) = ""
stringSudoku (Sudoku(x:xs)) = (toString x "") ++ stringSudoku (Sudoku xs)

toString :: Row -> String -> String
toString [] str             = str ++ "\n"
toString (Nothing : xs) str = toString xs (str ++ " " ++ ".")
toString ((Just n): xs) str = toString xs (str ++ " " ++ (show n))


-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku filepath = do
                      string <- readFile filepath
                      return (Sudoku (sudokuString [[] |_ <- [1..9] ] string))

sudokuString :: [Row] -> String -> [Row]
sudokuString rows "" = rows
sudokuString (r:rs) (c:cs) 
 | c == '\n' = r:(sudokuString rs cs)
 | c == '.' = sudokuString ((r ++ [Nothing]):rs) cs
 | c == ' ' = sudokuString (r:rs) cs
 | otherwise = sudokuString ((r ++ [(Just (digitToInt c))]):rs) cs

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [(9,chooseNothing),(1,chooseNumber)]

chooseNumber = elements [Just x| x <- [1..9]]

chooseNothing = elements [Nothing]

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
    rows <- vectorOf 9 $ vectorOf 9 cell
    return (Sudoku rows)

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sudoku = isSudoku sudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock block = repeated block

repeated :: [Cell] -> Bool
repeated [] = True
repeated (x:xs)
 | x == Nothing = repeated xs
 | otherwise = not (elem x xs) && repeated xs


-- * D2


blocks :: Sudoku -> [Block]
blocks sudoku = [createBlock sudoku (3*y) (3*x) | y <- [0..2], x <- [0..2]]

createBlock :: Sudoku -> Int -> Int -> Block
createBlock (Sudoku rows) y x = [rows !! k !! n | n <- [x..x+2], k <- [y..y+2]]

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths (Sudoku rows) = length (blocks (Sudoku rows)) == 9 
                                    && all (==True) block
    where block = [length n == 9 | n <- (blocks $ Sudoku rows)]

-- * D3

isOkay :: Sudoku -> Bool
isOkay (Sudoku rows) = rowBool rows && colBool [0..8] rows 
                       && blockBool (blocks (Sudoku rows))

blockBool :: [Block] -> Bool
blockBool [] = True
blockBool (x:xs) = repeated x && blockBool xs 

rowBool :: [Row] -> Bool
rowBool [] = True
rowBool (x:xs) = repeated x && rowBool xs

colBool :: [Int] -> [Row] -> Bool 
colBool [] rows = True
colBool (x:xs) rows = repeated (createCol rows x) && colBool xs rows

createCol :: [Row] -> Int -> Row
createCol rows int = [x !! int | x <- rows]


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
