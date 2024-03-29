module Sudoku where

import Test.QuickCheck
import Data.List
import Data.Char(digitToInt)
import Data.Maybe

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

example1 :: Sudoku
example1 =
    Sudoku
      [[],[],[],[],[],[],[],[],[]]
  where
    n = Nothing
    j = Just

testExample :: Sudoku
testExample =
    Sudoku
      [ [j 3,j 6,j 4,j 8,j 7,j 1,j 2,j 9,j 5]
      , [j 7,j 5,j 2,j 9,j 3,j 6,j 1,j 8,j 4]
      , [j 8,j 1,j 9,j 2,j 5,j 4,j 7,j 3,j 6]
      , [j 5,j 9,j 6,j 7,j 1,j 3,j 4,j 2,j 8]
      , [j 4,j 3,j 1,j 5,j 8,j 2,j 6,j 7,j 9]
      , [j 2,j 7,j 8,j 4,j 6,j 9,j 3,j 5,j 1]
      , [j 6,j 4,j 5,j 3,j 2,j 8,j 9,j 1,j 7]
      , [j 9,j 8,j 3,j 1,j 4,j 7,j 5,j 6,j 2]
      , [j 1,j 2,j 7,j 6,j 9,j 5,j 8,j 4,j 3]
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
isSudoku sudoku = all check1Row (rows sudoku) && length (rows sudoku) == 9

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
readSudoku :: FilePath -> IO Sudoku
readSudoku filepath = do
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
prop_blocks_lengths sudoku = length (blocks sudoku) == 27 && all (==True) block
    where block = [length n == 9 | n <- (blocks sudoku)]

-- * D3

isOkay :: Sudoku -> Bool
isOkay sudoku = (all repeated $ rows sudoku) && (all repeated $ blocks sudoku) && (all repeated $ createCol [] $ rows sudoku)

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
prop_blanks_allBlanks = [(n,k) | n <- [0..8], k <- [0..8]] == blanks allBlankSudoku


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
[] !!= _ = []
l@(x:[]) !!= (i,y)
 | i == 0    = [y]                           
 | otherwise = l
l@(x:xs) !!= (i,y)
 | i < 0                                        
 || i > (length l-1) = l                            
 | i == 0            = y : xs 
 | otherwise = x : (xs !!= (i-1, y))

prop_bangBangEquals_correct :: String -> (Int, Char) -> Bool
prop_bangBangEquals_correct s p@(i, c) 
 | i > length s -1 
 || i < 0     = (s !!= p) == s
 | otherwise  = ((s !!= p) !! i) == c 

-- * E3

--update :: Sudoku -> Pos -> Cell -> Sudoku
--update (Sudoku rows) (r,c) cell = Sudoku ( rows !!= (r,((rows !! r) !!= (c,cell))) )

update :: Sudoku -> Pos -> Cell -> Sudoku
update sud (r, c) _
 | r < 0 || c < 0 || r > 8 || c > 8 = sud 
update (Sudoku rs) pos c = Sudoku (rs !!= (fst pos, newR))
          where newR = (rs !! fst pos) !!= (snd pos,c)

prop_update_updated :: Sudoku -> Pos -> Cell -> Property 
prop_update_updated sud (row, col) c = abs row < 9 && abs col < 9 ==> pos (update sud p c) == c
          where p = (abs row, abs col)
                pos s = rows s !! abs row !! abs col

getCell :: Sudoku -> Pos -> Cell
getCell (Sudoku rows) (r,c) = ((rows!!r)!!c)


------------------------------------------------------------------------------

solve :: Sudoku -> Maybe Sudoku
solve sudoku
 | solutions /= [] = Just $ head solutions
 | otherwise = Nothing
  where solutions = solve' sudoku

solve' :: Sudoku -> [Sudoku]
solve' sudoku
 | null $ blanks sudoku = [sudoku] 
 | otherwise = concatMap solve' possibleBoards
  where b:bs = blanks sudoku
        possibleBoards = possibleMoves b sudoku

possibleMoves :: Pos -> Sudoku -> [Sudoku]
possibleMoves pos s = filter isOkay [update s pos (Just n) | n <- range]
    where range = [1..9]


readAndSolve :: FilePath -> IO ()
readAndSolve filepath = do
                        sudoku <- readSudoku filepath 
                        printSudoku $ fromJust (solve sudoku)

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf solution sudoku = ((map (getCell sudoku) keptPos) == 
  (map (getCell solution) keptPos)) && (isOkay solution) && (isFilled solution)
 where keptPos = ((blanks allBlankSudoku)\\(blanks sudoku))


prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = isSudoku s && isOkay s && isJust (solve s) ==> fromJust (solve s) `isSolutionOf` s