module Snake.GUI where

import Snake.Core
import Snake.IO

import Graphics.UI.WX
import Prelude hiding (Either(..))


-- | Start the main GUI
startGUI :: SnakeConfig -> IO ()
startGUI config = start $ gui config "lvl1.txt"


-- | Constant defining the update interval
updateInterval :: Int
updateInterval = 200


-- | Constants defining the pixel width of each square
-- on the grid.
squareWidth, squareHeight :: Int
squareWidth  = 10
squareHeight = 10


-- | GUI setup
gui :: SnakeConfig -> FilePath -> IO ()
gui config level = do

    g <- loadGrid level

    let gen     = randomGen config
        (g', _) = placeApple (placeSnake initialSnake g) gen
        (gridWidth, gridHeight) = gridDim g
        gridPxWidth  = gridWidth * squareWidth
        gridPxHeight = gridHeight * squareHeight

    -- | Our lovely grid.
    snakeGrid <- varCreate g'

    -- | Main frame and panel/canvas.
    mainFrame <- frameFixed [text := "Snake"]
    gridPanel <- panel mainFrame [on paint := paintMainPanel snakeGrid]

    -- | Key commands.
    set gridPanel [ on (charKey 'w') := changeDirection Up snakeGrid
                  , on upKey         := changeDirection Up snakeGrid

                  , on (charKey 's') := changeDirection Down snakeGrid
                  , on downKey       := changeDirection Down snakeGrid

                  , on (charKey 'a') := changeDirection Left snakeGrid
                  , on leftKey       := changeDirection Left snakeGrid

                  , on (charKey 'd') := changeDirection Right snakeGrid
                  , on rightKey      := changeDirection Right snakeGrid
                  ]

    quitBtn  <- button mainFrame [text := "Quit", on command := close mainFrame]
    resetBtn <- button mainFrame [text := "Reset"]

    -- | Timer, initiating a redraw every 'updateInterval' milliseconds.
    timer mainFrame [interval := updateInterval, on command := tick snakeGrid gridPanel]

    set mainFrame [layout := column 0
                      [ minsize (sz gridPxWidth gridPxHeight) $ widget gridPanel
                      , row 0 [widget quitBtn, widget resetBtn]
                      ]
                  ]

    return ()


-- | Performs a single tick of the application.
tick :: Var Grid -> Panel () -> IO ()
tick v p = do
    varUpdate v moveSnake
    repaint p


changeDirection :: Direction -> Var Grid -> IO ()
changeDirection d v = do
    snakeGrid <- varGet v
    let snake = gridSnake snakeGrid
    varSet v snakeGrid { gridSnake = setSnakeDirection d snake }


-- | Paint the main panel.
paintMainPanel :: Var Grid -> DC a -> Rect -> IO ()
paintMainPanel v dc _ = do
    g <- varGet v
    mapM_ (paintCell dc) $ enumerateCells g


-- | Paints an individual cell on the grid.
paintCell :: DC a -> (Coord, Cell) -> IO ()
paintCell dc ((y, x), cell) = paintCell' cell
    where coord = (y*squareHeight, x*squareWidth)
          paintCell' Empty     = return ()
          paintCell' Wall      = paintWallSegment dc coord
          paintCell' Apple     = paintApple dc coord
          paintCell' SnakePart = paintSnakePart dc coord


-- | Paints a wall segment on the grid.
paintWallSegment :: DC a -> Coord -> IO ()
paintWallSegment dc coord = do
    let (y, x) = coord
        r = rect (point x y) (sz squareWidth squareHeight)

    set dc [brushColor := black, brushKind := BrushSolid]
    drawRect dc r []

    -- putStrLn $ "Painted wall: " ++ show coord
    return ()


-- | Paints an apple on the grid.
paintApple :: DC a -> Coord -> IO ()
paintApple dc coord = do
    let (y, x) = coord
        r = rect (point x y) (sz squareWidth squareHeight)

    set dc [brushColor := red, brushKind := BrushSolid]
    drawRect dc r []

    -- putStrLn $ "Painted Apple: " ++ show coord
    return ()


-- | Paints a part of the snake on the grid.
paintSnakePart :: DC a -> Coord -> IO ()
paintSnakePart dc coord = do
    let (y, x) = coord
        r = rect (point x y) (sz squareWidth squareHeight)

    set dc [brushColor := green, brushKind := BrushSolid]
    drawRect dc r []

    -- putStrLn $ "Painted Snake: " ++ show coord
    return ()
