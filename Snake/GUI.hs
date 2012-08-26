module Snake.GUI where

import Snake.Core

import Graphics.UI.WX

-- | Start the main GUI
startGUI :: SnakeConfig -> IO ()
startGUI config = start $ gui config

gridWidth, gridHeight :: Int
gridWidth = 400
gridHeight = 400

-- | GUI setup
gui :: SnakeConfig -> IO ()
gui config = do
    let gen    = randomGen config
        (g, _) = placeApple initialGrid gen

    mainFrame <- frameFixed [text := "Snake"]
    gridPanel <- panel mainFrame [on paint := paintMainPanel g]

    quitBtn  <- button mainFrame [text := "Quit"]
    resetBtn <- button mainFrame [text := "Reset"]

    -- timer mainFrame [interval := 20, on command := repaint gridPanel]
    set   mainFrame [layout := column 0
                        [ minsize (sz gridWidth gridHeight) $ widget gridPanel
                        , row 0 [widget quitBtn, widget resetBtn]
                        ]
                    ]

    return ()

-- | Paint the main panel
paintMainPanel :: Grid -> DC a -> Rect -> IO ()
paintMainPanel g dc _ = mapM_ (paintCell dc) $ enumerateCells g

paintCell :: DC a -> (Coord, Cell) -> IO ()
paintCell _  (_,     Empty)     = return ()
paintCell dc (coord, Wall)      = paintWallSegment dc coord
paintCell dc (coord, Apple)     = paintApple dc coord
paintCell dc (coord, SnakePart) = paintSnakePart dc coord

paintWallSegment :: DC a -> Coord -> IO ()
paintWallSegment dc coord = do
    let (y, x) = coord
        r = rect (point (x*10) (y*10)) (sz 10 10)

    set dc [brushColor := black, brushKind := BrushSolid]
    drawRect dc r []

    -- putStrLn $ "Painted wall: " ++ show coord
    return ()

paintApple :: DC a -> Coord -> IO ()
paintApple dc coord = do
    let (y, x) = coord
        r = rect (point (x*10) (y*10)) (sz 10 10)

    set dc [brushColor := red, brushKind := BrushSolid]
    drawRect dc r []

    -- putStrLn $ "Painted Apple: " ++ show coord
    return ()

paintSnakePart :: DC a -> Coord -> IO ()
paintSnakePart dc coord = do
    let (y, x) = coord
        r = rect (point (x*10) (y*10)) (sz 10 10)

    set dc [brushColor := green, brushKind := BrushSolid]
    drawRect dc r []

    -- putStrLn $ "Painted Snake: " ++ show coord
    return ()
