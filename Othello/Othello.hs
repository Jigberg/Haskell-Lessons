module Othello where

import Test.QuickCheck
import Data.List
import Data.Char
import Data.Maybe

--------------------

{-

   !! HOW TO PLAY !!
   1. call "main" and type ai if you want to play against an ai. Anything else will allow input for both Black and White.
   2. Simply type the actions you want to do, no need for ( ).
   3. Typing:  "h" = Gives all the places where you can place a piece   "q" = Quits the game early
   4. To place you type for example "32".  3 is the row,  2 is the column ,  We start at 0.
   5. The game ends when the board is full or one color can't place. 


-}

--------------------

data Color = White | Black
 deriving ( Show, Eq )
opposite White = Black
opposite Black = White

type Cell = Maybe Color
type Row = [Cell]
type Pos = (Int,Int)

data Board = Board [Row]
 deriving ( Show, Eq )


rows :: Board -> [Row]
rows (Board ms) = ms

startBoard :: Board
startBoard = 
    Board
    [ [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
    , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
    , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
    , [n  ,n  ,n  ,j b,j w,n  ,n  ,n  ]
    , [n  ,n  ,n  ,j w,j b,n  ,n  ,n  ]
    , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
    , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
    , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
    ]
    where n = Nothing
          w = White
          b = Black
          j = Just





-- Nifty funtions
------------------


-- Print the current Othello board.
printBoard :: Board -> IO ()
printBoard b = putStr $ concat $ map listToString (rows b)

listToString :: [Cell] -> String
listToString cList = (intersperse ' ' (concat $ map cellToString cList)) ++ "\n"

cellToString :: Cell -> String
cellToString cell
 | cell == (Just Black) = "b"
 | cell == (Just White) = "w"
 | otherwise = "."


allPositions :: [Pos]
allPositions = [(n, k) | n <- [0..7], k <- [0..7]]

-- Checks if the board is 8x8 in size. 
boardSizeValid :: Board -> Bool
boardSizeValid b = (length r) == 8 && all (==8) (map length r)
  where r = rows b


-- Return Aligning row positions, even outside the board
directionList :: Int -> Int -> [[Pos]]
directionList r col = [ 
  [((r-x),col) | x <- l],
  [((r-x),(col+x)) | x <- l ] ,
  [(r,(col+x)) | x <- l ] ,
  [((r+x),(col+x)) | x <- l ] ,
  [((r+x),(col)) | x <- l ] ,
  [((r+x),(col-x)) | x <- l ] ,
  [((r),(col-x)) | x <- l ] ,
  [((r-x),(col-x)) | x <- l ] ]
   where l = [1..8]



-- Return empty positions
blanks :: Board -> [Pos]
blanks (Board rows) = [(n, k) | n <- [0..7], k <- [0..7], rows !! n !! k == Nothing]

-- Return occupied positions
nonBlanks :: Board -> [Pos]
nonBlanks (Board rows) = allPositions\\blanks (Board rows)


-- Returns the positions of all the cells of one color.
colorCells :: Board -> Color -> [Pos]
colorCells b c = filter color $ nonBlanks b
  where color pos = (getCell b pos) == (Just c)


-- Return the name of a color
colorName :: Color -> String
colorName c
 | c == White = "White"
 | otherwise = "Black"


-- Get the content of a position
getCell :: Board -> Pos -> Cell
getCell (Board rows) (r,c)
 | r < 0 || r > 7 || c < 0 || c > 7 = Nothing
 | otherwise = ((rows!!r)!!c)


-- replaces something in a list at a given index
replace :: [a] -> (Int,a) -> [a]
replace list (i,item) = fst (splitAt i list) ++ [item] ++ (tail $ snd (splitAt i list))


isBoard :: Board -> Bool
isBoard b = all check1Row (rows b) && length (rows b) == 8

check1Row :: Row -> Bool
check1Row row = all checkCell row && length row == 8

checkCell :: Cell -> Bool
checkCell content
 | content == Nothing || content == (Just Black) || content == (Just White) = True
 | otherwise = False


-- The main game loop
---------------------------



main :: IO ()
main = do
       putStrLn "Type 'ai' to play against computer / leave blank for 1vs1"
       input <- getLine
       if isSubsequenceOf "ai" input 
         then gameLoop startBoard White True
         else gameLoop startBoard White False


gameLoop :: Board -> Color -> Bool -> IO ()
gameLoop b c ai = do
                  printBoard b
                  putStrLn (colorName c ++ " turn, choose a postition.")
                  input <- getLine
                  loop b c ai input


loop :: Board -> Color -> Bool -> String -> IO ()
loop b c ai s
 | elem 'h' s = messageRestart b c ai (show (validPlacements b c) ++ "\n")
 | (length (validPlacements b c) == 0) = putStrLn (colorName c ++ " can't play, " ++ colorName (opposite c) ++ " is the Winner!")
 | (length $ blanks b) == 0 || (elem 'q' s) = declareWinner b
 | (length $ inputList s) < 2 = messageRestart b c ai "Sorry, I didn't understand that.\n"
 | not(elem (findPos $ inputList s) $ validPlacements b c) = messageRestart b c ai "You can't place there\n"
 | not(ai) = gameLoop newBoard (opposite c) ai
 | (length (validPlacements newBoard Black)) == 0 = putStrLn "Black can't play, White is the Winner"
 | otherwise = aiTurn b c ai s
  where newBoard  = flipPieces (placePiece b c (findPos $ inputList s)) c (findPos $ inputList s) 0

messageRestart :: Board -> Color -> Bool -> String -> IO ()
messageRestart b c ai s = do
                             putStr s
                             gameLoop b c ai

aiTurn :: Board -> Color -> Bool -> String -> IO ()
aiTurn b c ai s = do
                printBoard b
                putStr "Black has placed\n"
                gameLoop ( aiTakeTurn (flipPieces (placePiece b c (findPos $ inputList s)) c (findPos $ inputList s) 0) (opposite c) ) c ai

-- Chooses on of the valid positions to place on.
aiTakeTurn :: Board -> Color -> Board
aiTakeTurn b c = (flipPieces (placePiece b c (aiPos b c) ) c (aiPos b c) ) 0

-- Can be changed to be more random
aiPos :: Board -> Color -> Pos
aiPos b c = head $ validPlacements b c

inputList :: String -> [Int]
inputList s = map digitToInt $ filter isDigit s


-- Declares the color with the most pieces the winner or if there is a tie!
declareWinner :: Board -> IO ()
declareWinner b
 | count White > count Black = message White
 | count White < count Black = message Black
 | otherwise = putStrLn "It is a tie!"
  where count c = length $ colorCells b c
        message c = putStrLn ( (colorName c) ++ " is the Winner!  White - " ++ show (count White) ++ "  Black - " ++ show (count Black) )

-- Returns a postion
findPos :: [Int] -> Pos
findPos list = ((list!!0),(list!!1))



-- Tests - QuickCheck 
-----------------



rStartPosition :: Gen (Pos)
rStartPosition = do
                 x <- elements [0..6]
                 y <- elements [0..6]
                 return (x,y)

rColor :: Gen (Color)
rColor = elements [White,Black]

instance Arbitrary Board where
  arbitrary = do
    pos <- rStartPosition
    c <- rColor
    return (placeSquare (Board (replicate 8 (replicate 8 Nothing))) pos c 3)


placeSquare :: Board -> Pos -> Color -> Int -> Board
placeSquare b (row,col) c (-1) = b
placeSquare b (row,col) c i = placeSquare ( placePiece b c (posList!!i) ) (row,col) (opposite c) (i-1)
  where posList = [(row,col+1),(row,col),(row+1,col),(row+1,col+1)]


prop_Board :: Board -> Bool
prop_Board b = isBoard b


prop_play :: Board -> Bool
prop_play b = aiPlay b White


aiPlay :: Board -> Color -> Bool
aiPlay b c
 | length (validPlacements b c) == 0 = True
 | length (validPlacements b c) /= 0 = aiPlay (flipPieces (placePiece b c ((validPlacements b c)!!0)) c ((validPlacements b c)!!0) 0 ) (opposite c)
 | otherwise = False


prop_updatedBoard :: Board -> Bool
prop_updatedBoard b = isBoard (flipPieces (placePiece b White ((validPlacements b White)!!0)) White ((validPlacements b White)!!0) 0 )

-- Updates the board
---------------



-- Updates the board with a new piece.
placePiece :: Board -> Color -> Pos -> Board
placePiece b c (r,col) = Board(replace (rows b) (r, (replace (rows b !! r) (col,Just(c)))))


-- Flips all pieces that should be flipped given a position and color. 
flipPieces :: Board -> Color -> Pos -> Int -> Board
flipPieces b c (r,col) 8 = b
flipPieces b c (r,col) i = flipPieces (flipList b c (createflipList b c  ((directionList r col) !!i) [] )) c (r,col) (i+1)


-- Creates a list for 1 direction of pieces needed to be flipped.
createflipList :: Board -> Color -> [Pos] -> [Pos] -> [Pos]
createflipList b c (x:xs) new
 | getCell b x == (Just (opposite c)) = createflipList b c xs (new++[x])
 | getCell b x == (Just c) = new
 | otherwise = []

-- Flips/("places") all postions in a list. 
flipList :: Board -> Color -> [Pos] -> Board
flipList b c [] = b
flipList b c (x:xs) = flipList (placePiece b c x) c xs





-- Finds valid places for a color to be placed
-------------




-- Returns all the places where a new colored piece can be placed. 
validPlacements :: Board -> Color -> [Pos]
validPlacements b c = filter canFlip $ semiValid b c
  where canFlip pos = flipCheck b c pos 0


-- Checks if a placement can flip any pieces in on of the 8 directions. 
flipCheck :: Board -> Color -> Pos -> Int -> Bool
flipCheck b c (r,col) 8 = False
flipCheck b c (r,col) i
 | lineCheck b c ((directionList r col)!!i) = True
 | otherwise = flipCheck b c (r,col) (i+1)


-- Checks if a placement will flip any pieces in a given list.
lineCheck :: Board -> Color -> [Pos] -> Bool
lineCheck b c (x:xs)
 | getCell b x == Just (opposite c) = lineCheck' b c xs
 | otherwise = False

lineCheck' :: Board -> Color -> [Pos] -> Bool
lineCheck' b c [] = False
lineCheck' b c (x:xs)
 | getCell b x == (Just c) = True
 | getCell b x == Just (opposite c) = lineCheck' b c xs
 | otherwise = False


-- Returns all possible placements, Includes the ones that doesn't flip a the opponents pieces.
semiValid :: Board -> Color -> [Pos]
semiValid b c = unique [] (filter empty (semiValid' b $ colorCells b (opposite c)))
   where empty pos = (getCell b pos) == Nothing

semiValid' :: Board -> [Pos] -> [Pos]
semiValid' b []     = []
semiValid' b (x:xs) = surrounding x ++ semiValid' b xs

-- Removes all duplicates from a list
unique :: Eq a => [a] -> [a] -> [a]
unique x [] = x 
unique [] (a:xs) = unique [a] xs
unique x (a:xs) = if a `elem` x then unique x xs else unique (a:x) xs

-- Returns a list of the surrounding cells on the board.
surrounding :: Pos -> [Pos]
surrounding (r,c) = filter validPos square
  where square = [(x,y) | x <- [r-1..r+1], y <- [c-1..c+1]]\\[(r,c)]
        validPos (a,b) = elem a [0..7] && elem b [0..7]
