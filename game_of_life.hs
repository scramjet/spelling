{-# LANGUAGE BangPatterns #-}

import Prelude hiding (null)
import Control.Monad (forM_)
import Data.Maybe
import Data.Set (Set, member, fromList, null, empty)
import System.Environment (getArgs)
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (free)
import Control.Concurrent (threadDelay)
import UI.Nanocurses.Curses (initCurses, refresh, wMove, stdScr, 
                             waddnstr, getYX, endWin, resetParams, getCh)

type Point = (Int, Int)
type Board = Set Point

boardWidth  = 10
boardHeight = 10

xCoords = [0..boardWidth - 1]
yCoords = [0..boardHeight - 1]

allPoints :: [Point]
allPoints = [(x, y) | y <- yCoords, x <- xCoords]

makeBoard :: [Point] -> Board
makeBoard points = fromList points

isCellLive :: Board -> Point -> Bool
isCellLive board point = point `member` board

nextBoard :: Board -> Board
nextBoard board = makeBoard [point | point <- allPoints, succIsLive point]
  where
    succIsLive point = nextCell (isLive point) (liveNeighbours point)

    liveNeighbours point = 
      length . filter id . map isLive . neighbouringPts $ point

    neighbouringPts (x, y) = 
      [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= dy || dx /= 0]

    isLive = isCellLive board

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
matrix2Board rows = 
  makeBoard . livePoints . concat $ rowsWithPoints rows 0
  where
    rowsWithPoints :: [[Char]] -> Int -> [[(Char, Point)]]
    rowsWithPoints [] _ = []
    rowsWithPoints (row:rows) y  = 
      (rowWithPoints row y) : rowsWithPoints rows (y + 1)

    rowWithPoints :: [Char] -> Int -> [(Char, Point)]
    rowWithPoints row y = zipWith (\c x -> (c, (x, y))) row [0..]

    livePoints :: [(Char, Point)] -> [Point]
    livePoints = map snd . filter ((==) 'X' . fst)

printBoard :: Board -> IO ()
printBoard board = mapM_ putStrLn $ board2Matrix board

printConsole :: [Board] -> IO ()
printConsole boards = forM_ boards printFrame
  where printFrame board = do
          printBoard board
          putStrLn $ replicate boardWidth '*'

printCurses :: [Board] -> IO ()
printCurses boards = do
  forM_ boards showFrame
  where 
    showFrame board = do
      wMove stdScr 2 5
      forM_ (board2Matrix board) showLine
      refresh
      wait

    showLine line = do
      (y, x) <- getYX stdScr
      showStr line
      wMove stdScr (y + 1) 5

    showStr str = do
      cStr <- newCString str
      waddnstr stdScr cStr (fromIntegral . length $ str)
      free cStr

    wait = threadDelay (2 * 100000) -- or getCh

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

boardOscillators = 
  matrix2Board [" X        ",
                " X        ",
                " X        ",
                "          ",
                "          ",
                "          ",
                "    XXX   ",
                "  XXX     ",
                "          ",
                "          "]

boardGliders = 
  matrix2Board ["X         ",
                " XX       ",
                "XX        ",
                "          ",
                " X        ",
                " XX       ",
                "X X       ",
                "          ",
                "          ",
                "          "]

boardQueenBee = 
  matrix2Board ["          ",
                "XX        ",
                "  X       ",
                "   X      ",
                "   X      ",
                "   X      ",
                "  X       ",
                "XX        ",
                "          ",
                "          "]
boardOscillators2 = 
  matrix2Board ["          ",
                "XXXXXXXXXX",
                "          ",
                "          ",
                "    X     ",
                "   XXX    ",
                "   X X    ",
                "   XXX    ",
                "    X     ",
                "          "]


standardBoards = 
  [("oscillators1", boardOscillators), 
   ("oscillators2",boardOscillators2), 
   ("gliders", boardGliders),
   ("board1", board1), 
   ("queenBee", boardQueenBee)]

main = do
  [name] <- getArgs

  case lookup name standardBoards of
    Just board -> runBoard board
    Nothing    -> showHelp name
  
  where 
    runBoard board = do
      initCurses (return ())
      printCurses (games board)
      endWin

    showHelp name = do
      putStrLn $ "No board called " ++ name
      putStr "Boards: "
      putStrLn . show . map fst $ standardBoards