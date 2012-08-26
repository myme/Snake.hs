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
    return $ Grid (width, height) cells

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

gridString :: String
gridString = unlines [ "################################"
                     , "#                              #"
                     , "#                              #"
                     , "#                              #"
                     , "#                              #"
                     , "#                              #"
                     , "#                              #"
                     , "#                              #"
                     , "#                              #"
                     , "################################"
                     ]

enumerateCells :: Grid -> [(Coord, Cell)]
enumerateCells g = zip coords $ gridCells g
    where (width, height) = gridDim g
          coords = [(y, x) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

emptyCoords :: Grid -> [Coord]
emptyCoords = map fst . filter ((== Empty) . snd) . enumerateCells

placeApple :: (RandomGen g) => Grid -> g -> (Grid, g)
placeApple g r = (setCell coord Apple g, r')
    where coord     = empty !! idx
          (idx, r') = randomR (0, length empty) r
          empty     = emptyCoords g

placeSnake :: Snake -> Grid -> Grid
placeSnake s g = foldr (`setCell` SnakePart) g $ snakeBody s

setCell :: Coord -> Cell -> Grid -> Grid
setCell c v g = g { gridCells = cells }
    where cells = map f (enumerateCells g)
          f (c', v') = if c == c' then v else v'

initialGrid :: Grid
initialGrid = fromJust $ readGrid gridString

initialSnake :: Snake
initialSnake = Snake Right [(2, 4), (2, 3), (2, 2)]

snakeLength :: Snake -> Int
snakeLength = length . snakeBody
