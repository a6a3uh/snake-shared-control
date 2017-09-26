module Snake (stepSnake, commandSnake) where

import Control.Lens
import Control.Monad.State
import Linear.V2
import World
import Food

-- todo add food move at startup

stepSnake :: State World ()
stepSnake = do
    world <- get
    if world ^. isOver
    then put world
    else do modify moveSnake
            eaten <- eatFood
            if eaten
            then moveFood
            else modify gameOver

gameOver :: World -> World
gameOver w = let (x:xs) = w ^. snake
             in if inBounds x &&            -- inside world
                   (x `notElem` xs)         -- not eaten itself
                then w else w & isOver .~ True

moveSnake :: World -> World
moveSnake world = case compare (world ^. stomack) 0 of
                    LT -> world & snake %~ init   & stomack %~ succ -- check for init of empty list !!!
                    GT -> world & snake %~ (pos:) & stomack %~ pred
                    EQ -> world & snake %~ (\xs -> pos : init xs)
    where pos = let (V2 x y) = head (world ^. snake)
                in case world ^. direction of
                    North -> V2 x (y + 1)
                    East  -> V2 (x + 1)  y
                    South -> V2 x (y - 1)
                    West  -> V2 (x - 1) y

commandSnake :: Direction -> World -> World
commandSnake dir world = commandSnake' dir $ over (table . traverse . reward) pred world
  where commandSnake' North = over direction (\d -> if d == South then South else North)
        commandSnake' East  = over direction (\d -> if d == West  then West  else East)
        commandSnake' South = over direction (\d -> if d == North then North else South)
        commandSnake' West  = over direction (\d -> if d == East  then East  else West)
