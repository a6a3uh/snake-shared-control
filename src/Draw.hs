{-# LANGUAGE TemplateHaskell #-}

module Draw where

import Graphics.Gloss.Interface.Pure.Game
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Snake
import World

data GameWorld = NewGameWorld
  { _snakeWorld :: World
  , _resolution :: (Int, Int)
  }

makeLenses ''GameWorld

displayMode :: GameWorld -> Display
displayMode gworld = InWindow "Snake" (gworld ^. resolution) (0, 0)

---------------------------------------

handleEvent :: Event -> Game GameWorld Settings Log ()
handleEvent event = do
    gworld <- get
    case event of
        EventResize newResolution   -> handleResize newResolution
        EventKey key state' _ _     -> if gworld ^. snakeWorld . isOver
            then return ()
            else handleKey key state'
        _ -> return ()

handleStep :: Float -> Game GameWorld Settings Log ()
handleStep _time = do
    conf <- ask
    if conf ^. game . auto
    then zoom snakeWorld stepSnake
    else return ()

handleResize :: (Int, Int) -> Game GameWorld Settings Log ()
handleResize newResolution = modify (& resolution .~ newResolution)

handleKey :: Key -> KeyState -> Game GameWorld Settings Log ()
handleKey key state' = do
    conf <- ask
    let cmd = if conf ^. game . direct then commandSnake else commandMarkov
    case state' of
        Down -> case key of
            SpecialKey KeyUp    -> zoom snakeWorld (cmd North)
            SpecialKey KeyRight -> zoom snakeWorld (cmd East)
            SpecialKey KeyDown  -> zoom snakeWorld (cmd South)
            SpecialKey KeyLeft  -> zoom snakeWorld (cmd West)
            SpecialKey KeySpace -> do
                if conf ^. game . auto
                then return ()
                else zoom snakeWorld stepSnake
            _ -> return ()
        _ -> return ()

---------------------------------------

drawWorld :: Int -> GameWorld -> Picture
drawWorld sc gworld = pictures
    [ drawBounds gworld
    , drawTable sc gworld
    , drawSnake sc gworld
    , drawGameOver gworld
    ]

drawBounds :: GameWorld -> Picture
drawBounds gworld = rectangleWire x x
    where x = size' gworld + 1

drawSnake :: Int -> GameWorld -> Picture
drawSnake sc gworld = case gworld ^. snakeWorld . snake of
    (p : ps) -> pictures
        ( color orange (drawBox sc gworld p)
        : map (drawBox sc gworld) ps
        )
    _        -> blank

drawTable :: Int -> GameWorld -> Picture
drawTable sc gworld = color green $ mconcat foodPictures
  where foodPictures = drawFood sc gworld <$> gworld ^. snakeWorld . table

drawFood :: Int -> GameWorld -> Food -> Picture
drawFood sc gworld food = let box = drawBox sc gworld (food ^. place)
                              txt = drawText sc food gworld (food ^. place) --G.color G.red (G.scale 0.2 0.2 (G.text $ (show . reward . apples) world))
                          in mappend box txt

drawBox :: Int -> GameWorld -> Pos -> Picture
drawBox sc gworld = drawPos sc (rectangleUpperSolid s s) gworld
    where s = size' gworld / fromIntegral sc - 2

drawText :: Int -> Food -> GameWorld -> Pos -> Picture
drawText sc food = drawPos sc (color red (scale 0.1 0.1 (text . show $ food ^. reward)))

drawPos :: Int -> Picture -> GameWorld -> Pos -> Picture
drawPos sc pic gworld (x, y) =
    let s = size' gworld / fromIntegral sc
        x' = s * fromIntegral x
        y' = s * fromIntegral y
    in  translate x' y' pic

drawGameOver :: GameWorld -> Picture
drawGameOver gworld = if world ^. isOver
    then pictures
        [ color red (scale 0.2 0.2 (text "game over"))
        , color blue (translate 0 (-50) (scale 0.2 0.2 (text ("score: " ++ show (length (world ^. snake))))))
        ]
    else blank
    where world = gworld ^. snakeWorld

---------------------------------------

backgroundColor :: Color
backgroundColor = white

size' :: (Num a) => GameWorld -> a
size' gworld =
    let (width, height) = gworld ^. resolution
    in  fromIntegral (min width height)
