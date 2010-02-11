{-# LANGUAGE BangPatterns #-}

import Prelude hiding (null)
import Data.Set (Set, member, fromList, null, empty)

type Point = (Int, Int)
type Board = Set Point

boardWidth  = 10
boardHeight = 10

xCoords = [0..boardWidth - 1]
yCoords = [0..boardHeight - 1]

board1 = 
  matrix2Board ["X XX X XXX",
                "X X X X X ",
                "X X   X X ",
                "X X X X X ",
                "X X X  XX ",
                "X XX    X ",
                "X X X   X ",
                "X   X X X ",
                "X X   X X ",
                "X X X   X "]

board'glider = 
  matrix2Board ["          ",
                "  X       ",
                "   XX     ",
                "  XX      ",
                "          ",
                "          ",
                "          ",
                "          ",
                "          ",
                "          "]

allPoints :: [Point]
allPoints = [(x, y) | y <- xCoords, x <- yCoords]

makeBoard :: [Point] -> Board
makeBoard points = fromList points

isCellLive :: Board -> Point -> Bool
isCellLive board point = point `member` board

nextBoard :: Board -> Board
nextBoard board = makeBoard [point | point <- allPoints, succIsLive point]
  where
    succIsLive point = 
      nextCell (isCellLive board point) (liveNeighbours point)

    liveNeighbours = 
      length. filter id . map (isCellLive board) . neighbouringPts

    neighbouringPts (x, y) = 
      [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= dy || dx > 0]

nextCell :: Bool -> Int -> Bool
nextCell True neighbours  | neighbours < 2  = False
                          | neighbours > 3  = False
                          | otherwise       = True
nextCell False neighbours | neighbours == 3 = True
                          | otherwise       = False

games :: Board -> [Board]
games board | null board = []
            | otherwise = board : games (nextBoard board)

board2Matrix :: Board -> [[Char]]
board2Matrix board = [[cell (x, y) | x <- xCoords] | y <- yCoords]
  where cell p = if isCellLive board p then 'X' else ' '

matrix2Board :: [[Char]] -> Board
matrix2Board m = 
  makeBoard . map toPoint . filter isLive $ zip (concat m) allPoints
  where toPoint (_, p) = p
        isLive (c, _) = c == 'X'

printBoard :: Board -> IO ()
printBoard board = mapM_ putStrLn $ board2Matrix board

printGames :: Board -> IO ()
printGames board = mapM_ printFrame $ games board
    where 
      printFrame b = do
        printBoard b
        putStrLn $ replicate boardWidth '*'

main :: IO ()
main = printGames board1
