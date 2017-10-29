module Snake (stepSnake, commandSnake, commandMarkov) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Maybe
import Data.List
import Markov
import Dynamic
import World
import Food

-- todo add food move at startup

stepSnake :: Game World Settings' ()
stepSnake = do
    world <- get
    if world ^. isOver
    then return ()
    else stepWorld

stepWorld :: Game World Settings' ()
stepWorld = do 
    world <- get
    conf <- ask
    if conf ^. gameSettings . direct
    then moveSnake (world ^. direction) 
    else do 
        d <- dirMarkov
        moveSnake d
    eaten <- eatFood
    if eaten
    then moveFood
    else gameOver

gameOver :: Game World Settings' ()
gameOver = do
    world <- get
    conf <- ask
    let (x:xs) = world ^. snake
    if inBounds (conf ^. gameSettings . dimentions) x                   -- inside world
        && ((x `notElem` xs) || conf ^. gameSettings . cross)           -- not eaten itself
    then return ()
    else put $ world & isOver .~ True

inBounds :: (Int, Int) -> Pos Int -> Bool
inBounds (xm, ym) (x, y) =
    let xms = xm `div` 2
        yms = ym `div` 2
    in  -xms <= x && x <= xms && -yms <= y && y <= yms

moves :: Int -> Int -> [(Int, Int)]
moves x y = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

moveSnake :: Direction -> Game World a ()
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


dirMarkov :: Game World Settings' Direction
dirMarkov = do 
    world <- get
    conf <- ask
    let (x, y) = head (world ^. snake)
        ps      = world ^.. table . traverse . place
        pr      = world ^.. table . traverse . prob
            
        minPossibleIndex xs cs =    let minIndex ys = fromJust $ elemIndex (minimum ys) ys
                                        i = minIndex cs 
                                    in  if conf ^. gameSettings . cross
                                        then toEnum i
                                        else
                                            if (moves x y !! i) `notElem` xs || cs !! i == 1000000000
                                            then toEnum i
                                            else minPossibleIndex xs (cs & ix i .~ 1000000000)
    
    costs'   <- lift $ magnify dynamicSettings $ markovOut (x, y) ps pr
                        
    tell $ "MARKOV OUT> costs: " ++ show costs' ++ "\n"
    
    return $ minPossibleIndex (world ^. snake) costs' 


commandMarkov :: Direction -> Game World Settings' ()
commandMarkov dir = do 
    world <- get

    let pr = world ^.. table . traverse . prob
        ps = world ^.. table . traverse . place
        p  =  head $ world ^. snake
        
    newProbs <- lift $ magnify dynamicSettings $ markovIn p ps pr (fromEnum dir)

    let changeProbs foods = fmap (\(a, p') -> a & prob .~ p') $ zipWith (,) foods newProbs
    
    put $ world & table . traverse . reward %~ pred 
                & table %~ changeProbs

    world' <- get

    tell $ "COMMAND> " ++ show dir ++ "; " ++ "probabilities: " ++ (show $ world' ^.. table . traverse . prob) ++ "\n"

commandSnake :: Direction -> Game World Settings' ()
commandSnake dir = do 
    world <- get
    put $ commandSnake' dir $ over (table . traverse . reward) pred world
    where   commandSnake' North = over direction (\d -> if d == South then South else North)
            commandSnake' East  = over direction (\d -> if d == West  then West  else East)
            commandSnake' South = over direction (\d -> if d == North then North else South)
            commandSnake' West  = over direction (\d -> if d == East  then East  else West)
