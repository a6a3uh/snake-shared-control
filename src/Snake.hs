module Snake (stepSnake, commandSnake) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import Data.List
import Markov
import World
import Food

-- todo add food move at startup

stepSnake :: StateT World (Writer String) ()
stepSnake = do
    world <- get
    if world ^. isOver
    then put world
    else do moveSnake
            eaten <- eatFood
            if eaten
            then moveFood
            else modify gameOver

gameOver :: World -> World
gameOver w = let (x:xs) = w ^. snake
             in if    inBounds x           -- inside world
                   && (x `notElem` xs)         -- not eaten itself
                then w else w & isOver .~ True
                

moveSnake :: StateT World (Writer String) ()
moveSnake = do 
    world <- get
    let (x, y) = head (world ^. snake)
        ps      = world ^.. table . traverse . place
        pr      = world ^.. table . traverse . prob
        costs   = markovOut (x, y) ps pr
        pos = newpos $ minPossibleIndex (world ^. snake) costs 
        newpos a = case a of 
            0 -> (x - 1, y)
            1 -> (x + 1, y)
            2 -> (x, y - 1)
            3 -> (x, y + 1)
        minPossibleIndex xs cs =    let minIndex ys = fromJust $ elemIndex (minimum ys) ys
                                        i = minIndex cs 
                                    in  if newpos i `notElem` xs || cs !! i == 1000000000
                                        then i
                                        else minPossibleIndex xs (cs & ix i .~ 1000000000)

    case compare (world ^. stomack) 0 of
        LT -> put $ world & snake %~ init   & stomack %~ succ -- check for init of empty list !!!
        GT -> put $ world & snake %~ (pos:) & stomack %~ pred
        EQ -> put $ world & snake %~ (\xs -> pos : init xs)

          
commandSnake :: Direction -> StateT World (Writer String) ()
commandSnake dir = do 
    world <- get
    let pr = world ^.. table . traverse . prob
        ps = world ^.. table . traverse . place
        p  =  head $ world ^. snake
        newProbs = markovIn p ps pr (fromEnum dir)
        changeProbs foods = fmap (\(a, p') -> a & prob .~ p') $ zipWith (,) foods newProbs
    tell "blah"
    put $ world & table . traverse . reward %~ pred 
                & table %~ changeProbs

                                  
--     commandSnake' dir $ over (table . traverse . reward) pred world
--   where commandSnake' North = over direction (\d -> if d == South then South else North)
--         commandSnake' East  = over direction (\d -> if d == West  then West  else East)
--         commandSnake' South = over direction (\d -> if d == North then North else South)
--         commandSnake' West  = over direction (\d -> if d == East  then East  else West)
