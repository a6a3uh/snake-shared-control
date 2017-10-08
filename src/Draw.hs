{-# LANGUAGE TemplateHaskell #-}

module Draw where

import Graphics.Gloss.Interface.Pure.Game
import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
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

handleEvent :: Event -> StateT GameWorld (Writer String) ()
handleEvent event = do
    gworld <- get
    case event of
        EventResize newResolution   -> handleResize newResolution
        EventKey key state' _ _     -> if gworld ^. snakeWorld . isOver
            then put gworld
            else handleKey key state'
        _ -> put gworld

handleStep :: Float -> StateT GameWorld (Writer String) ()
handleStep _time = zoom snakeWorld stepSnake

handleResize :: (Int, Int) -> StateT GameWorld (Writer String) ()
handleResize newResolution = modify (& resolution .~ newResolution)

handleKey :: Key -> KeyState -> StateT GameWorld (Writer String) ()
handleKey key state' = do
    gworld <- get
    case state' of
        Down -> case key of
            SpecialKey KeyUp    -> zoom snakeWorld (commandSnake North)
            SpecialKey KeyRight -> zoom snakeWorld (commandSnake East)
            SpecialKey KeyDown  -> zoom snakeWorld (commandSnake South)
            SpecialKey KeyLeft  -> zoom snakeWorld (commandSnake West)
            _ -> put gworld
        _ -> put gworld

---------------------------------------

drawWorld :: GameWorld -> Picture
drawWorld gworld = pictures
    [ drawBounds gworld
    , drawTable gworld
    , drawSnake gworld
    , drawGameOver gworld
    ]

drawBounds :: GameWorld -> Picture
drawBounds gworld = rectangleWire x x
    where x = size gworld + 1

drawSnake :: GameWorld -> Picture
drawSnake gworld = case gworld ^. snakeWorld . snake of
    (p : ps) -> pictures
        ( color orange (drawBox gworld p)
        : map (drawBox gworld) ps
        )
    _        -> blank

drawTable :: GameWorld -> Picture
drawTable gworld = color green $ mconcat foodPictures
  where foodPictures = drawFood gworld <$> gworld ^. snakeWorld . table

drawFood :: GameWorld -> Food -> Picture
drawFood gworld food = let box = drawBox gworld (food ^. place)
                           txt = drawText food gworld (food ^. place) --G.color G.red (G.scale 0.2 0.2 (G.text $ (show . reward . apples) world))
                       in mappend box txt

drawBox :: GameWorld -> Pos -> Picture
drawBox gworld = drawPos (rectangleUpperSolid s s) gworld
    where s = size gworld / fromIntegral (worldScale settings) - 2

drawText :: Food -> GameWorld -> Pos -> Picture
drawText food = drawPos (color red (scale 0.1 0.1 (text . show $ food ^. reward)))

drawPos :: Picture -> GameWorld -> Pos -> Picture
drawPos pic gworld (x, y) =
    let s = size gworld / fromIntegral (worldScale settings)
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

size :: (Num a) => GameWorld -> a
size gworld =
    let (width, height) = gworld ^. resolution
    in  fromIntegral (min width height)
