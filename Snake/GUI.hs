module Snake.GUI where

import Snake.Core
import Snake.IO

import Graphics.UI.WX


-- | Start the main GUI
startGUI :: SnakeConfig -> IO ()
startGUI config = start $ gui config "lvl1.txt"


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

    mainFrame <- frameFixed [text := "Snake"]
    gridPanel <- panel mainFrame [on paint := paintMainPanel g']

    quitBtn  <- button mainFrame [text := "Quit", on command := close mainFrame]
    resetBtn <- button mainFrame [text := "Reset"]

    -- timer mainFrame [interval := 20, on command := repaint gridPanel]
    set mainFrame [layout := column 0
                      [ minsize (sz gridPxWidth gridPxHeight) $ widget gridPanel
                      , row 0 [widget quitBtn, widget resetBtn]
                      ]
                  ]

    return ()

-- | Paint the main panel.
paintMainPanel :: Grid -> DC a -> Rect -> IO ()
paintMainPanel g dc _ = mapM_ (paintCell dc) $ enumerateCells g


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
