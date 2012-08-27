module Snake.Core where

import Control.Monad (guard)
import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (swap)
import Prelude hiding (Either(..))
import System.Random


--------------------------------------------------
--
-- Datastructures
--

data SnakeState = SnakeState
                { stateGrid :: Grid
                , randomGen :: StdGen
                }

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
tick :: SnakeState -> SnakeState
tick (SnakeState oldGrid gen) = fromMaybe (SnakeState oldGrid gen) newState

    where dim        = gridDim oldGrid
          grownSnake = wrapSnake dim . growSnake $ gridSnake oldGrid
          (lastCoord, movedSnake) = reduceTail grownSnake
          nextCoord = head $ snakeBody grownSnake
          grownGrid = (setCell nextCoord SnakePart oldGrid)   { gridSnake = grownSnake }
          movedGrid = (setCell lastCoord Empty     grownGrid) { gridSnake = movedSnake }
          newState  = do
              cellType <- getCell nextCoord oldGrid
              case cellType of
                  Wall      -> Nothing
                  SnakePart -> Nothing
                  Apple     -> Just $ uncurry SnakeState $ placeApple grownGrid gen
                  Empty     -> Just $ SnakeState movedGrid gen


-- | Wraps the snake to the other side of the grid if
-- it exceeds the grid dimensions.
wrapSnake :: Dimension -> Snake -> Snake
wrapSnake (w, h) snake = snake { snakeBody = newBody }
    where oldBody   = snakeBody snake
          newBody   = wrapCoord (head oldBody) : tail oldBody
          wrapCoord (y, x)
              | y  < 0    = wrapCoord (h - 1, x)
              | y >= h    = wrapCoord (0, x)
              | x  < 0    = wrapCoord (y, w - 1)
              | x >= w    = wrapCoord (y, 0)
              | otherwise = (y, x)


-- | Grows the snake by one cell in its current direction
-- of movement. The caller will have to check if the new
-- location is in fact valid.
growSnake :: Snake -> Snake
growSnake snake = snake { snakeBody = newBody }
    where (y, x)    = head $ snakeBody snake
          newBody   = nextCoord : snakeBody snake
          nextCoord = case snakeDir snake of
                          Up    -> (y - 1, x)
                          Down  -> (y + 1, x)
                          Left  -> (y, x - 1)
                          Right -> (y, x + 1)


-- | Reduces/pops off the last cell of the snake (its tail).
-- Returns a tuple of the removed coordinate and the updated snake.
reduceTail :: Snake -> (Coord, Snake)
reduceTail snake = (lastCoord, snake { snakeBody = newBody })
    where newBody   = init $ snakeBody snake
          lastCoord = last $ snakeBody snake


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
