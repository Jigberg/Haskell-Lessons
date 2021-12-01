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
isSudoku (Sudoku rows) = all check1Row rows

check1Row :: Row -> Bool
check1Row row = all checkCell row && length row == 9

checkCell :: Cell -> Bool
checkCell Nothing = True
checkCell (Just n) = n >= 1 && n <= 9

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku rows) = not $ all (elem Nothing) rows

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
getSudoku :: FilePath -> IO Sudoku
getSudoku filepath = do
                      string <- readFile filepath
                      return (Sudoku (checkSudoku [[] |_ <- [1..9] ] string))

checkSudoku :: [Row] -> String -> [Row]
checkSudoku rows string = if isSudoku $ Sudoku (sudokuString rows string) then sudokuString rows string
                     else error "Shit, that aint a Sudoku!"

sudokuString :: [Row] -> String -> [Row]
sudokuString rows "" = rows
sudokuString (r:rs) (c:cs) 
 | c == '\n' = r:(sudokuString rs cs)
 | c == '.' = sudokuString ((r ++ [Nothing]):rs) cs
 | otherwise = sudokuString ((r ++ [(Just (digitToInt c))]):rs) cs

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [(9,chooseNothing),(1,chooseNumber)]

chooseNumber = elements [Just x| x <- [1..9]]

chooseNothing = return Nothing

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
 | otherwise = x `notElem` xs && repeated xs


-- * D2


blocks :: Sudoku -> [Block]
blocks sudoku = [createBlock sudoku (3*y) (3*x) | y <- [0..2], x <- [0..2]]++ rows sudoku ++ createCol [] (rows sudoku)

createBlock :: Sudoku -> Int -> Int -> Block
createBlock (Sudoku rows) y x = [rows !! k !! n | n <- [x..x+2], k <- [y..y+2]]

createCol :: [Row] -> [Row] -> [Row]
createCol col row
  | length col == 9 = col
  | otherwise = createCol (col ++ [map head row]) (map tail row) 


prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths (Sudoku rows) = length (blocks (Sudoku rows)) == 9 
                                    && all (==True) block
    where block = [length n == 9 | n <- (blocks $ Sudoku rows)]

-- * D3

isOkay :: Sudoku -> Bool
isOkay (Sudoku rows) = all repeated rows

---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks (Sudoku rows) = [ (n, k) | n <- [0..8], k <- [0..8], rows !! n !! k == Nothing ]

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = length (blanks allBlankSudoku) == 81


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = (take i first) ++ (y:last)
               where (first,last) = splitAt (i+1) xs

prop_bangBangEquals_correct :: (Eq a) => [a] -> (Int, a) -> Bool
prop_bangBangEquals_correct list (int, a) = ((list !!= (modint, a)) !! modint) == a
    where modint = int `mod` (length list + 1)


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku rows) (r,c) cell = Sudoku ( rows !!= (r,((rows !! r) !!= (c,cell))) )

prop_update_updated :: Sudoku -> Pos -> Cell -> Bool
prop_update_updated (Sudoku rows) (r,c) cell = getCell (update (Sudoku rows) (r,c) cell) (r,c) == cell 

getCell :: Sudoku -> Pos -> Cell
getCell (Sudoku rows) (r,c) = ((rows!!r)!!c)


------------------------------------------------------------------------------

-- * F1

-- disposal =  blanks getcell update isOkay update

--solve :: Sudoku -> Maybe Sudoku
--solve sudoku = 

-- * F2

-- Sort from Blanks, get keep track where we are in that list.
-- When pos is placed with a rand number (of available number), keep going.
-- If no fit go back to the last spot and change number.


-- Just 0 for "no viable answer found". 
getOkayCell :: Sudoku -> Pos -> Int -> Cell
getOkayCell sudoku pos n
 | isOkay $ update sudoku pos $ numberPicker n = numberPicker n
 |numberPicker n == (Just 0) = Just 0
 | otherwise = getOkayCell sudoku pos (n+1) 

numberPicker :: Int -> Cell
numberPicker n = if 8 < n then (Just 0)
                 else [Just x| x <- [1..9]]!!n



-- * F3


-- * F4


solve take function