module Snake.Core where

import Control.Monad (guard)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Prelude hiding (Either(..))
import System.Random


--------------------------------------------------
--
-- Datastructures
--

data Grid = Grid
          { gridDim :: Dimension
          , gridCells :: [Cell]
          , gridSnake :: Snake
          }

type Dimension = (Int, Int) -- ^ (width, height)

data Cell = Empty | Wall | Apple | SnakePart
          deriving (Eq)

data Snake = Snake
           { snakeDir :: Direction
           , snakeBody :: [Coord]
           } deriving (Show)

data Direction = Up | Down | Left | Right
               deriving (Show)

type Coord = (Int, Int) -- ^ (y, x)

data SnakeConfig = SnakeConfig { randomGen :: StdGen }


--------------------------------------------------
--
-- Initialization functions
--

emptyGrid :: Dimension -> Grid
emptyGrid d@(w, h) = Grid d (replicate (w * h) Empty) emptySnake


emptySnake :: Snake
emptySnake = Snake Right []


addBorder :: Grid -> Grid
addBorder g = g { gridCells = map fn (enumerateCells g) }
    where (width, height) = gridDim g
          fn ((y, x), c) = if y == 0 || y == height - 1 ||
                              x == 0 || x == width - 1
                               then Wall
                               else c

initialSnake :: Snake
initialSnake = Snake Right [(2, 4), (2, 3), (2, 2)]


--------------------------------------------------
--
-- Accessor/modification functions
--

-- | Returns the length of the snake.
snakeLength :: Snake -> Int
snakeLength = length . snakeBody


-- | Generic function to wrap 'head' in Maybe.
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe xs = Just $ head xs


-- | Returns the cell type at the given coordinate.
getCell :: Coord -> Grid -> Maybe Cell
getCell c = headMaybe . map snd . filter ((== c) . fst) . enumerateCells


-- | Sets the cell type at the given coordinate.
setCell :: Coord -> Cell -> Grid -> Grid
setCell c v g = g { gridCells = cells }
    where cells = map f (enumerateCells g)
          f (c', v') = if c == c' then v else v'


-- | Returns the list of cells enumerated with their coordinate.
enumerateCells :: Grid -> [(Coord, Cell)]
enumerateCells g = zip coords $ gridCells g
    where (width, height) = gridDim g
          coords = [(y, x) | y <- [0 .. height - 1], x <- [0 .. width - 1]]


-- | Returns all cells of the given type.
cellsOfType :: Cell -> Grid -> [Coord]
cellsOfType c = map fst . filter ((== c) . snd) . enumerateCells


-- | Returns a list of coordinates of all empty cells.
emptyCells :: Grid -> [Coord]
emptyCells = cellsOfType Empty


-- | Randomly places an apple on one of the empty cells of the grid.
placeApple :: (RandomGen g) => Grid -> g -> (Grid, g)
placeApple g r = (setCell coord Apple g, r')
    where coord     = empty !! idx
          (idx, r') = randomR (0, length empty) r
          empty     = emptyCells g


-- | Adds the snake to the grid.
placeSnake :: Snake -> Grid -> Grid
placeSnake s g = g' { gridSnake = s }
    where g' = foldr (`setCell` SnakePart) g $ snakeBody s


-- | Sets the new direction the snake will travel in.
setSnakeDirection :: Direction -> Snake -> Snake
setSnakeDirection d s = s { snakeDir = d }


-- | Moves the snake one step within the grid.
--
-- The next cell might be:
--
--  * Empty - Snake moves one step forward.
--  * Wall  - Snake collides and dies.
--  * Apple - Snake moves one step forward, eats apple and
--            grows one cell in length (tail is not retracted).
--  * Snake - Snake collides with itself and dies.
moveSnake :: Grid -> Grid
moveSnake g = newGrid { gridSnake = newSnake }

    where snake = gridSnake g

          -- | The cell to be remove from the snake.
          lastCoord = last $ snakeBody snake

          -- | The coordinates of the next cell.
          nextCoord =
              let (y, x) = head $ snakeBody snake
                  (w, h) = gridDim g

              in  case snakeDir snake of
                      Up    -> let y' = y - 1
                               in  if y' < 0
                                       then (h - 1, x)
                                       else (y - 1, x)
                      Down  -> let y' = y + 1
                               in  if y' >= h
                                       then (0, x)
                                       else (y + 1, x)
                      Left  -> let x' = x - 1
                               in  if x' < 0
                                       then (y, w - 1)
                                       else (y, x - 1)
                      Right -> let x' = x + 1
                               in  if x' >= w
                                       then (y, 0)
                                       else (y, x + 1)

          -- | The snake after the move.
          newSnake = case getCell nextCoord g of
                         Nothing        -> snake
                         Just Empty     -> snake { snakeBody = moveForward }
                         Just Wall      -> snake
                         Just Apple     -> snake { snakeBody = grow }
                         Just SnakePart -> snake
              where grow = nextCoord : snakeBody snake
                    moveForward = init grow

          -- | The grid after the move.
          -- Add new snake squares and remove the old ones.
          newGrid  = foldr ($) g
                   [ setCell nextCoord SnakePart
                   , setCell lastCoord Empty
                   ]


--------------------------------------------------
--
-- Instances
--

instance Show Grid where
    show = showGrid

instance Show Cell where
    show c = showCell c : ""


--------------------------------------------------
--
-- Serialization
--

showGrid :: Grid -> String
showGrid g = unlines . showLines $ gridCells g
    where (width, _) = gridDim g
          showLines [] = []
          showLines xs = let (l, r) = splitAt width xs
                         in  map showCell l : showLines r

showCell :: Cell -> Char
showCell = fromJust . flip lookup cellToChrMap

readGrid :: String -> Maybe Grid
readGrid "" = Nothing
readGrid s  = do
    let ls     = filter (not . null) . lines $ s
        width  = length . head $ ls
        height = length ls

    guard $ all ((== width) . length) ls
    cells <- sequence . concatMap (map readCell) $ ls

    return $ Grid (width, height) cells initialSnake

readCell :: Char -> Maybe Cell
readCell = flip lookup chrToCellMap

chrToCellMap :: [(Char, Cell)]
chrToCellMap = [ (' ', Empty)
               , ('#', Wall)
               , ('@', Apple)
               , ('o', SnakePart)
               ]

cellToChrMap :: [(Cell, Char)]
cellToChrMap = map swap chrToCellMap
