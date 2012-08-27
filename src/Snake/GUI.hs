module Snake.GUI where

import Snake.Core
import Snake.IO

import Graphics.UI.WX
import Prelude hiding (Either(..))
import System.Random


-- | Start the main GUI
startGUI :: StdGen -> FilePath -> IO ()
startGUI gen level = start $ gui gen level


-- | Constant defining the update interval
updateInterval :: Int
updateInterval = 100


-- | Constants defining the pixel width of each square
-- on the grid.
squareWidth, squareHeight :: Int
squareWidth  = 10
squareHeight = 10


-- | GUI setup
gui :: StdGen -> FilePath -> IO ()
gui gen level = do

    g <- loadGrid level

    let (g', gen') = placeApple (placeSnake initialSnake g) gen
        (gridWidth, gridHeight) = gridDim g
        gridPxWidth  = gridWidth * squareWidth
        gridPxHeight = gridHeight * squareHeight

    -- | Our lovely grid.
    snakeState <- varCreate $ SnakeState g' gen'

    -- | Main frame and panel/canvas.
    mainFrame <- frameFixed [text := "Snake"]
    gridPanel <- panel mainFrame [on paint := paintMainPanel snakeState]

    -- | Key commands.
    set gridPanel [ on (charKey 'w') := changeDirection Up snakeState
                  , on upKey         := changeDirection Up snakeState

                  , on (charKey 's') := changeDirection Down snakeState
                  , on downKey       := changeDirection Down snakeState

                  , on (charKey 'a') := changeDirection Left snakeState
                  , on leftKey       := changeDirection Left snakeState

                  , on (charKey 'd') := changeDirection Right snakeState
                  , on rightKey      := changeDirection Right snakeState
                  ]

    quitBtn  <- button mainFrame [text := "Quit", on command := close mainFrame]
    resetBtn <- button mainFrame [text := "Reset"]

    -- | Timer, initiating a redraw every 'updateInterval' milliseconds.
    timer mainFrame [interval := updateInterval, on command := guiTick snakeState gridPanel]

    set mainFrame [layout := column 0
                      [ minsize (sz gridPxWidth gridPxHeight) $ widget gridPanel
                      , row 0 [widget quitBtn, widget resetBtn]
                      ]
                  ]

    return ()


-- | Performs a single tick of the application.
guiTick :: Var SnakeState -> Panel () -> IO ()
guiTick v p = do
    varUpdate v tick
    repaint p


changeDirection :: Direction -> Var SnakeState -> IO ()
changeDirection d v = do
    snakeState <- varGet v
    let snakeGrid = stateGrid snakeState
        snake = gridSnake snakeGrid
    varSet v snakeState { stateGrid = snakeGrid { gridSnake = setSnakeDirection d snake } }


-- | Paint the main panel.
paintMainPanel :: Var SnakeState -> DC a -> Rect -> IO ()
paintMainPanel v dc _ = do
    snakeState <- varGet v
    let g = stateGrid snakeState
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
