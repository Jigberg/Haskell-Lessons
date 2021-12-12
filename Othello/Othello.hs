module Sudoku where

import Test.QuickCheck
import Data.List
import Data.Char(digitToInt)
import Data.Maybe

-- 8x8 , 32 brickor vadare , pattern in the begining, can only place so that a color changes , 

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
    , [n  ,n  ,j w,n  ,n  ,n  ,n  ,n  ]
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

-- All of the boards positions
blanks :: Board -> [Pos]
blanks (Board rows) = [ (n, k) | n <- [0..7], k <- [0..7], rows !! n !! k == Nothing ]

nonBlanks :: Board -> [Pos]
nonBlanks (Board rows) = [ (n, k) | n <- [0..7], k <- [0..7], rows !! n !! k /= Nothing ]


getCell :: Board -> Pos -> Cell
getCell (Board rows) (r,c) = ((rows!!r)!!c)


-- Returns all possible placements for a new piece, depending on the color. 
--validPlacements :: Color -> [Pos]


-- Returns all the places where a new colored piece can be placed. 
validPlacements :: Board -> Color -> [Pos]
validPlacements b c = filter canFlip $ semiValid b c
  where canFlip pos = flipCheck b c pos

-------------

-- Checks if the placement can flip any pieces.
flipCheck :: Board -> Color -> Pos -> Bool
flipCheck b c pos = horizontal b c pos -- && vertical b c pos && diagonal b c pos

-- Checks if a possible placement can flip any pieces horizontaly.
horizontal :: Board -> Color -> Pos -> Bool
horizontal b c (r,col)
 | getCell b (r,(col-1)) == Just (opposite c) && (col-2)>(-1) = lineCheck b c $ reverse $ filter color [(r,x) | x <- [0..col]]
 | getCell b (r,(col+1)) == Just (opposite c) && (col+2)<(8) = lineCheck b c $ filter color [(r,x) | x <- [col..7]]
 | otherwise = False
  where color pos = (getCell b pos) == (Just c)

-- Checks if a hor/ver/dia line from position x can result in a flip. 
lineCheck :: Board -> Color -> [Pos] -> Bool
lineCheck b c [] = False
lineCheck b c (x:xs)
 | getCell b x == (Just c) = True
 | getCell b x == Just (opposite c) = lineCheck b c xs
 | otherwise = False

-------------

-- Returns all semi possible placements, includes the ones that doesn't flip a the opponents pieces. 
semiValid :: Board -> Color -> [Pos]
semiValid b c = filter empty (semiValid' b c $ colorCells b c)
   where empty pos = (getCell b pos) == Nothing

semiValid' :: Board -> Color -> [Pos] -> [Pos]
semiValid' b c []     = []
semiValid' b c (x:xs) = surrounding x ++ semiValid' b c xs


-- Returns the positions of all the cells of one color.
colorCells :: Board -> Color -> [Pos]
colorCells b c = filter color $ nonBlanks b
  where color pos = (getCell b pos) == (Just c)

-- Get a list of the surrounding cells.
surrounding :: Pos -> [Pos]
surrounding (r,c) = filter validPos square
  where square = [(x,y) | x <- [r-1..r+1], y <- [c-1..c+1]]\\[(r,c)]
        validPos (a,b) = elem a [0..8] && elem b [0..8]


