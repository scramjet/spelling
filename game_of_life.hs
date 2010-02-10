import Data.Set (Set, member, fromList)
import Data.List (foldl')

type Point = (Int, Int)
type Board = Set Point

boardWidth  = 20
boardHeight = 20

cellState :: Board -> Point -> Bool
cellState board point = point `member` board

nextBoard :: Board -> Board
nextBoard board = makeBoard $ [point | point <- points, isLive point]
  where
    points = [(x, y) | x <- [0..boardWidth], y <- [0..boardHeight]]

    isLive point = nextState (cellState board point) (neighbourCount point)

    neighbourCount = sum . liveNeighbours

    liveNeighbours = map (bool2int . cellState board) . neighbours

    neighbours :: Point -> [Point]
    neighbours (x, y) = [(x + dx, y + dy) | dx <- [-1, 1], dy <- [-1, 1]]

    bool2int b = if b then 1 else 0

nextState :: Bool -> Int -> Bool
nextState True neighbours  | neighbours < 2  = False
                           | neighbours > 3  = False
                           | otherwise       = True
nextState False neighbours | neighbours == 3 = True
                           | otherwise       = False

makeBoard :: [Point] -> Board
makeBoard points = fromList points

printBoard :: Board -> String
printBoard = undefined
