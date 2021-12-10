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

-- Returns the positions of all the cells of one color.
colorCells :: Board -> Color -> [Pos]
colorCells b c = filter color $ nonBlanks b
  where color pos = (getCell b pos) == (Just c)

-- Get a list of the surrounding cells.
surrounding :: Pos -> [(Pos)]
surrounding (r,c) = filter validPos square
  where square = [(x,y) | x <- [r-1..r+1], y <- [c-1..c+1]]\\[(r,c)]
        validPos (a,b) = elem a [0..8] && elem b [0..8]


