module Snake where

import Control.Monad (guard)
import Data.Maybe (fromJust)
import Prelude hiding (Either(..))

data Grid = Grid
          { gridDim :: (Int, Int)
          , gridCells :: [Cell]
          } deriving (Show)

data Cell = Empty | Wall | Apple | SnakePart
          deriving (Show, Eq)

data Snake = Snake
           { snakeDir :: Direction
           , snakeBody :: [Coord]
           } deriving (Show)

data Direction = Up | Down | Left | Right
               deriving (Show)

type Coord = (Int, Int) -- ^ (y, x)

chrToCellMap :: [(Char, Cell)]
chrToCellMap = [ (' ', Empty)
               , ('#', Wall)
               , ('@', Apple)
               , ('o', SnakePart)
               ]

cellToChrMap :: [(Cell, Char)]
cellToChrMap = [(y, x) | (x, y) <- chrToCellMap]

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
                     , "#                              #"
                     , "#                              #"
                     , "################################"
                     ]

readGrid :: String -> Maybe Grid
readGrid s = do
    let ls     = lines s
        width  = length . head $ ls
        height = length ls
    guard $ all ((== width) . length) ls
    cells <- sequence . concatMap (map readCell) $ ls
    return $ Grid (width, height) cells

readCell :: Char -> Maybe Cell
readCell = flip lookup chrToCellMap

-- showGrid :: Grid -> String
-- showGrid = 

showCell :: Cell -> Char
showCell = fromJust . flip lookup cellToChrMap

initialGrid :: Grid
initialGrid = fromJust $ readGrid gridString

initialSnake :: Snake
initialSnake = Snake Right [(0, 0), (0, 1), (0, 2)]

snakeLength :: Snake -> Int
snakeLength = length . snakeBody
