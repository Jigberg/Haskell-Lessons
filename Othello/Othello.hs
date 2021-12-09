module Sudoku where

import Test.QuickCheck
import Data.List
import Data.Char(digitToInt)
import Data.Maybe

-- 8x8 , 32 brickor vadare , pattern in the begining, can only place so that a color changes , 

data Color = White | Black
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

-- Returns all possible placements for a new piece, depending on the color. 
--validPlacements :: Color -> [Pos]

-- Returns all the empty cells surrounding the opposite color.
borderPos :: Board -> Color -> [Pos]
borderPos b c = 

-- Get a list of the surrounding cells.
surrounding :: Pos -> [(Pos)]
surrounding (r,c) = filter validPos square
  where square = [(x,y) | x <- [r-1..r+1], y <- [c-1..c+1]]\\[(r,c)]

validPos :: (Pos) -> Bool
validPos (r,c) = elem r [0..8] && elem c [0..8]


