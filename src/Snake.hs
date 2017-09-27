module Snake (stepSnake, commandSnake) where

import Control.Lens
import Control.Monad.State
import Data.Maybe
import Data.List
import Markov
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
             in if    inBounds x           -- inside world
                --    && (x `notElem` xs)         -- not eaten itself
                then w else w & isOver .~ True
                

moveSnake :: World -> World
moveSnake world = case compare (world ^. stomack) 0 of
                    LT -> world & snake %~ init   & stomack %~ succ -- check for init of empty list !!!
                    GT -> world & snake %~ (pos:) & stomack %~ pred
                    EQ -> world & snake %~ (\xs -> pos : init xs)
    where pos = let (x, y) = head (world ^. snake)
                    ps = world ^.. table . traverse . place
                    pr = world ^.. table . traverse . prob
                    costs = markovOut (x, y) ps pr
                in case fromJust $ elemIndex (minimum costs) costs of 
                        0 -> (x - 1, y)
                        1 -> (x + 1, y)
                        2 -> (x, y - 1)
                        3 -> (x, y + 1)
                    -- case world ^. direction of
                    -- North -> V2 x (y + 1)
                    -- East  -> V2 (x + 1)  y
                    -- South -> V2 x (y - 1)
                    -- West  -> V2 (x - 1) y

commandSnake :: Direction -> World -> World
commandSnake dir world = world 
                            & table . traverse . reward %~ pred 
                            & table %~ changeProbs
    where changeProbs foods = let pr = world ^.. table . traverse . prob
                                  ps = world ^.. table . traverse . place
                                  p  =  head $ world ^. snake
                                  newProbs = markovIn p ps pr (fromEnum dir)
                              in  fmap (\(a, p') -> a & prob .~ p') $ zipWith (,) foods newProbs
                                  
--     commandSnake' dir $ over (table . traverse . reward) pred world
--   where commandSnake' North = over direction (\d -> if d == South then South else North)
--         commandSnake' East  = over direction (\d -> if d == West  then West  else East)
--         commandSnake' South = over direction (\d -> if d == North then North else South)
--         commandSnake' West  = over direction (\d -> if d == East  then East  else West)
