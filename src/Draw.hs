{-# LANGUAGE TemplateHaskell #-}

module Draw where

import qualified Graphics.Gloss.Interface.Pure.Game as G
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

displayMode :: GameWorld -> G.Display
displayMode gworld = G.InWindow "Snake" (gworld ^. resolution) (0, 0)

---------------------------------------

handleEvent :: G.Event -> StateT GameWorld (Writer String) () -- GameWorld -> GameWorld
handleEvent event = do
    gworld <- get
    case event of
        G.EventResize newResolution -> handleResize newResolution
        G.EventKey key state' _ _ -> if gworld ^. snakeWorld . isOver
            then put gworld
            else handleKey key state'
        _ -> put gworld

handleGameStep :: Float -> StateT GameWorld (Writer String) () -- GameWorld -> GameWorld
handleGameStep _time = zoom snakeWorld stepSnake
    -- put $ gworld & snakeWorld .~ (liftM (snd . fst . runWriter) . runStateT) stepSnake (gworld ^. snakeWorld)

handleResize :: (Int, Int) -> StateT GameWorld (Writer String) () -- GameWorld -> GameWorld
handleResize newResolution = do
    gworld <- get
    put $ gworld & resolution .~ newResolution

handleKey :: G.Key -> G.KeyState -> StateT GameWorld (Writer String) () --World -> World
handleKey key state' = do
    gworld <- get
    case state' of
        G.Down -> case key of
            G.SpecialKey G.KeyUp    -> zoom snakeWorld (commandSnake North) -- (liftM (snd . fst . runWriter) . runStateT) (commandSnake North) gworld
            G.SpecialKey G.KeyRight -> zoom snakeWorld (commandSnake East) -- (liftM (snd . fst . runWriter) . runStateT) (commandSnake East)  gworld
            G.SpecialKey G.KeyDown  -> zoom snakeWorld (commandSnake South) -- (liftM (snd . fst . runWriter) . runStateT) (commandSnake South) gworld
            G.SpecialKey G.KeyLeft  -> zoom snakeWorld (commandSnake West) -- (liftM (snd . fst . runWriter) . runStateT) (commandSnake West)  gworld
            _ -> put gworld
        _ -> put gworld

---------------------------------------

drawWorld :: GameWorld -> G.Picture
drawWorld gworld = G.pictures
    [ drawBounds gworld
    , drawTable gworld
    , drawSnake gworld
    , drawGameOver gworld
    ]

drawBounds :: GameWorld -> G.Picture
drawBounds gworld = G.rectangleWire x x
    where x = size gworld + 1

drawSnake :: GameWorld -> G.Picture
drawSnake gworld = case gworld ^. snakeWorld . snake of
    (p : ps) -> G.pictures
        ( G.color G.orange (drawBox gworld p)
        : map (drawBox gworld) ps
        )
    _        -> G.blank

drawTable :: GameWorld -> G.Picture
drawTable gworld = G.color G.green $ mconcat foodPictures
  where foodPictures = drawFood gworld <$> gworld ^. snakeWorld . table

drawFood :: GameWorld -> Food -> G.Picture
drawFood gworld food = let box = drawBox gworld (food ^. place)
                           txt = drawText food gworld (food ^. place) --G.color G.red (G.scale 0.2 0.2 (G.text $ (show . reward . apples) world))
                       in mappend box txt

drawBox :: GameWorld -> Pos -> G.Picture
drawBox gworld = drawPos (G.rectangleUpperSolid s s) gworld
    where s = size gworld / fromIntegral (worldScale settings) - 2

drawText :: Food -> GameWorld -> Pos -> G.Picture
drawText food = drawPos (G.color G.red (G.scale 0.1 0.1 (G.text . show $ food ^. reward)))

drawPos :: G.Picture -> GameWorld -> Pos -> G.Picture
drawPos pic gworld (x, y) =
    let s = size gworld / fromIntegral (worldScale settings)
        x' = s * fromIntegral x
        y' = s * fromIntegral y
    in  G.translate x' y' pic

drawGameOver :: GameWorld -> G.Picture
drawGameOver gworld = if world ^. isOver
    then G.pictures
        [ G.color G.red (G.scale 0.2 0.2 (G.text "game over"))
        , G.color G.blue (G.translate 0 (-50) (G.scale 0.2 0.2 (G.text ("score: " ++ show (length (world ^. snake))))))
        ]
    else G.blank
    where world = gworld ^. snakeWorld

---------------------------------------

backgroundColor :: G.Color
backgroundColor = G.white

size :: (Num a) => GameWorld -> a
size gworld =
    let (width, height) = gworld ^. resolution
    in  fromIntegral (min width height)
