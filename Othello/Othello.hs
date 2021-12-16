module Othello where

import Test.QuickCheck
import Data.List
import Data.Char(digitToInt)
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
printBoard b = putStr $ boardToString b

boardToString :: Board -> String
boardToString (Board []) = ""
boardToString (Board(x:xs)) = (listToString x "") ++ boardToString (Board xs)

listToString :: [Cell] -> String -> String
listToString [] str = str ++ "\n"
listToString (Nothing:xs) str = listToString xs (str ++ " " ++ ".")
listToString ((Just White):xs) str = listToString xs (str ++ " " ++ "w")
listToString ((Just Black):xs) str = listToString xs (str ++ " " ++ "b")


-- Checks if the board is 8x8 in size. 
boardSizeValid :: Board -> Bool
boardSizeValid b = (length r) == 8 && all (==8) (map length r)
  where r = rows b


-- Return empty positions
blanks :: Board -> [Pos]
blanks (Board rows) = [ (n, k) | n <- [0..7], k <- [0..7], rows !! n !! k == Nothing ]

-- Return occupied positions
nonBlanks :: Board -> [Pos]
nonBlanks (Board rows) = [ (n, k) | n <- [0..7], k <- [0..7], rows !! n !! k /= Nothing ]


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



-- The main game loop
---------------------------



main :: IO ()
main = do
       putStrLn "Type 'ai' to play against computer / leave blank for 1vs1"
       input <- getLine
       if isSubsequenceOf "ai" input 
         then playOthello startBoard White True
         else playOthello startBoard White False



-- The game loop. Can be started with an ai to play singlePlayer.
playOthello :: Board -> Color -> Bool -> IO ()
playOthello b c ai =

  do
  printBoard b
  putStrLn (colorName c ++ " turn, choose a postition.")
  input <- getLine

  -- Show the player where they can place
  if (elem 'h' input)
    then do
         putStr (show (validPlacements b c) ++ "\n")
         playOthello b c ai

     -- End the game if the color can't place
     else if (length (validPlacements b c) == 0) 
       then putStrLn (colorName c ++ " can't play, " ++ colorName (opposite c) ++ " is the Winner!")

        -- End the game if "q" or if the board is full
        else if (length $ blanks b) == 0 || (elem 'q' input)
          then declareWinner b
  
          -- If the input was unvalid, make them pick again
          else if (length $ inputList input) < 2
            then do 
                 putStr "Sorry, I didn't understand that.\n"
                 playOthello b c ai
                        
            -- If the spot is unvalid, make them pick again
            else if not(elem (findPos $ inputList input) $ validPlacements b c)
              then do
                   putStr "You can't place there\n"
                   playOthello b c ai
 
              -- If there is no Ai the other color gets to play
              else if ai /= True
                then playOthello (newBoard input) (opposite c) ai
                               
                -- Ends the game if the Ai can't play
                else if (length (validPlacements (newBoard input) Black)) == 0 
                  then putStrLn "Black can't play, White is the Winner"
                  else do
                       printBoard (newBoard input)
                       putStr "Black has placed\n"
                       playOthello ( aiTakeTurn (newBoard input) (opposite c) ) c ai

  where inputList input = map digitToInt $ filterNumbers input ""
        newBoard input = flipPieces (placePiece b c (findPos $ inputList input)) c (findPos $ inputList input)0
                     



-- Chooses on of the valid positions to place on.
aiTakeTurn :: Board -> Color -> Board
aiTakeTurn b c = (flipPieces (placePiece b c (aiPos b c) ) c (aiPos b c) ) 0

-- Can be changed to be more random
aiPos :: Board -> Color -> Pos
aiPos b c = head $ validPlacements b c


-- Declares the color with the most pieces the winner or if there is a tie!
declareWinner :: Board -> IO ()
declareWinner b
 | (length $ colorCells b White) > (length $ colorCells b Black)
 = putStrLn ("White is the Winner!  Score: " ++ show (length $ colorCells b White) ++ " - " ++ show (length $ colorCells b Black))
 | (length $ colorCells b White) < (length $ colorCells b Black)
 = putStrLn ("Black is the Winner!  Score: " ++ show (length $ colorCells b Black) ++ " - " ++ show (length $ colorCells b White) )
 | otherwise = putStrLn "It is a tie!"

-- Returns a postion
findPos :: [Int] -> Pos
findPos list = ((list!!0),(list!!1))

-- Returns a string only containing digits.
filterNumbers :: String -> String -> String
filterNumbers [] new = reverse new
filterNumbers (x:xs) new
 | elem x ['0','1','2','3','4','5','6','7','8','9'] = filterNumbers xs ([x]++new)
 | otherwise = filterNumbers xs new




-- Updates the board
---------------



-- Updates the board with a new piece.
placePiece :: Board -> Color -> Pos -> Board
placePiece b c (r,col) = Board(replace (rows b) (r, (replace (rows b !! r) (col,Just(c)))))

-- Flips all pieces that should be flipped given a position and color. 
flipPieces :: Board -> Color -> Pos -> Int -> Board
flipPieces b c (r,col) i
 | i == 0 = flipPieces (flipList b c (createflipList b c  ([((r-x),col) | x <- [1..8]])  []))       c (r,col) (i+1) -- 12:00
 | i == 1 = flipPieces (flipList b c (createflipList b c  ([((r-x),(col+x)) | x <- [1..8] ])  []))  c (r,col) (i+1) -- 13:30
 | i == 2 = flipPieces (flipList b c (createflipList b c  ([(r,(col+x)) | x <- [1..8] ])  []))      c (r,col) (i+1) -- 15:00
 | i == 3 = flipPieces (flipList b c (createflipList b c  ([((r+x),(col+x)) | x <- [1..8] ])  []))  c (r,col) (i+1) -- 16:30
 | i == 4 = flipPieces (flipList b c (createflipList b c  ([((r+x),(col)) | x <- [1..8] ])  []))    c (r,col) (i+1) -- 18:00
 | i == 5 = flipPieces (flipList b c (createflipList b c  ([((r+x),(col-x)) | x <- [1..8] ])  []))  c (r,col) (i+1) -- 19:30
 | i == 6 = flipPieces (flipList b c (createflipList b c  ([((r),(col-x)) | x <- [1..8] ])  []))    c (r,col) (i+1) -- 21:00
 | i == 7 = flipPieces (flipList b c (createflipList b c  ([((r-x),(col-x)) | x <- [1..8] ])  []))  c (r,col) (i+1) -- 22:30
 | otherwise = b


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
flipCheck b c (r,col) 0 = if lineCheck b c [((r-x),col) | x <- [1..8]]     then True else flipCheck b c (r,col) 1 -- 12:00
flipCheck b c (r,col) 1 = if lineCheck b c [((r-x),(col+x)) | x <- [1..8]] then True else flipCheck b c (r,col) 2 -- 13:30
flipCheck b c (r,col) 2 = if lineCheck b c [(r,(col+x)) | x <- [1..8]]     then True else flipCheck b c (r,col) 3 -- 15:00
flipCheck b c (r,col) 3 = if lineCheck b c [((r+x),(col+x)) | x <- [1..8]] then True else flipCheck b c (r,col) 4 -- 16:30
flipCheck b c (r,col) 4 = if lineCheck b c [((r+x),(col)) | x <- [1..8]]   then True else flipCheck b c (r,col) 5 -- 18:00
flipCheck b c (r,col) 5 = if lineCheck b c [((r+x),(col-x)) | x <- [1..8]] then True else flipCheck b c (r,col) 6 -- 19:30
flipCheck b c (r,col) 6 = if lineCheck b c [((r),(col-x)) | x <- [1..8]]   then True else flipCheck b c (r,col) 7 -- 21:00
flipCheck b c (r,col) 7 = if lineCheck b c [((r-x),(col-x)) | x <- [1..8]] then True else flipCheck b c (r,col) 8 -- 22:30
flipCheck b c (r,col) 8 = False


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
