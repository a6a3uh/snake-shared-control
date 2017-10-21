module Snake (stepSnake, commandSnake, commandMarkov) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Maybe
import Data.List
import Markov
import World
import Food

-- todo add food move at startup

stepSnake :: Game World Settings1 Log ()
stepSnake = do
    world <- get
    if world ^. isOver
    then put world
    else stepWorld

stepWorld :: Game World Settings1 Log ()
stepWorld = do 
    world <- get
    conf <- ask
    if conf ^. game . direct
    then moveSnake (world ^. direction) 
    else do 
        d <- dirMarkov
        moveSnake d
    eaten <- eatFood
    if eaten
    then moveFood
    else gameOver

gameOver :: Game World Settings1 Log () --World -> World
gameOver = do
    world <- get
    conf <- ask
    let (x:xs) = world ^. snake
    if inBounds x           -- inside world
        && ((x `notElem` xs) || conf ^. game . cross)         -- not eaten itself
    then put world 
    else put $ world & isOver .~ True
      
moves :: Int -> Int -> [(Int, Int)]
moves x y = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

moveSnake :: Direction -> Game World a Log ()
moveSnake d = do
    world <- get
    let (x, y) = head (world ^. snake)
        pos    = moves x y !! fromEnum d
    case compare (world ^. stomack) 0 of
        LT -> put $ world & snake %~ init   & stomack %~ succ -- check for init of empty list !!!
        GT -> put $ world & snake %~ (pos:) & stomack %~ pred
        EQ -> put $ world & snake %~ (\xs -> pos : init xs)
    world' <- get
    tell $ "STEP> head: " ++ show (head $ world' ^. snake) ++ "\n"


dirMarkov :: Game World Settings1 Log Direction
dirMarkov = do 
    world <- get
    conf <- ask
    let (x, y) = head (world ^. snake)
        ps      = world ^.. table . traverse . place
        pr      = world ^.. table . traverse . prob
        costs'   = markovOut (x, y) ps pr
            
        minPossibleIndex xs cs =    let minIndex ys = fromJust $ elemIndex (minimum ys) ys
                                        i = minIndex cs 
                                    in  if conf ^. game . cross
                                        then toEnum i
                                        else
                                            if (moves x y !! i) `notElem` xs || cs !! i == 1000000000
                                            then toEnum i
                                            else minPossibleIndex xs (cs & ix i .~ 1000000000)
    
    tell $ "MARKOV OUT> costs: " ++ show costs' ++ "\n"
    
    return $ minPossibleIndex (world ^. snake) costs' 


commandMarkov :: Direction -> Game World Settings1 Log ()
commandMarkov dir = do 
    world <- get

    let pr = world ^.. table . traverse . prob
        ps = world ^.. table . traverse . place
        p  =  head $ world ^. snake
        newProbs = markovIn p ps pr (fromEnum dir)
        changeProbs foods = fmap (\(a, p') -> a & prob .~ p') $ zipWith (,) foods newProbs
    
    put $ world & table . traverse . reward %~ pred 
                & table %~ changeProbs

    world' <- get

    tell $ "COMMAND> " ++ show dir ++ "; " ++ "probabilities: " ++ (show $ world' ^.. table . traverse . prob) ++ "\n"

commandSnake :: Direction -> Game World Settings1 Log ()
commandSnake dir = do 
    world <- get
    put $ commandSnake' dir $ over (table . traverse . reward) pred world
    where   commandSnake' North = over direction (\d -> if d == South then South else North)
            commandSnake' East  = over direction (\d -> if d == West  then West  else East)
            commandSnake' South = over direction (\d -> if d == North then North else South)
            commandSnake' West  = over direction (\d -> if d == East  then East  else West)
