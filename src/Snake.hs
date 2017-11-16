module Snake (stepSnake, commandSnake, commandMarkov) where

import Control.Lens hiding (uncons)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import Data.List
import Markov
import Dynamic
import World
import Food

optimalCommand :: Pos Int -> [Food] -> Direction
optimalCommand p t = dirSelect p posrew
    where   rews = t ^.. traverse . reward
            maxrew = fromJust $ elemIndex (maximum rews) rews 
            posrew = (t !! maxrew) ^. place
            dirSelect (x0, y0) (x1, y1) | x1 - x0 >= 0 && x1 - x0 >= y1 - y0 = East
                                        | x1 - x0 <= 0 && x1 - x0 <= y1 - y0 = West
                                        | y1 - y0 >= 0 && y1 - y0 >= x1 - x0 = North
                                        | y1 - y0 <= 0 && y1 - y0 <= x1 - x0 = South

stepSnake :: Game World Settings' ()
stepSnake = do 
    world <- get
    conf <- ask
    when (world^.snake == []) (throwError ZeroLength)

    h <- maybe (throwError ZeroLength) return $ listToMaybe (world ^. snake)
    
    when (conf^.gameSettings.timeout > 0 && conf^.gameSettings.timeout < world^.stepCounter) (throwError TimeOut)
    put $ world & stepCounter %~ succ
    world <- get
    if conf^.gameSettings.direct
    then moveSnake (world^.direction) 
    else do 
        when (conf^.playerSettings.autoPlay) $ do
            if (conf^.playerSettings.commandOnStep /= 0) 
            then    if (world^.tick >= conf^.playerSettings.commandOnStep) 
                    then do
                        put $ world & tick.~0                        
                        commandMarkov $ optimalCommand h (world^.table)
                    else put $ world & tick %~ succ            
            else    if ((head $ world^.snake) `elem` (world^.visited)) 
                    then do
                        put $ world & visited .~ []
                        commandMarkov $ optimalCommand h (world^.table)
                    else put $ world & visited %~ (h:)
        dirMarkov >>= moveSnake
    eaten <- eatFood
    if eaten
    then do
        moveFood
        world' <- get
        put $ world' & visited .~ []
    else checkGameOver

checkGameOver :: Game World Settings' ()
checkGameOver = do
    world <- get
    conf <- ask
    (x,xs) <- maybe (throwError ZeroLength) return $ uncons (world ^. snake)
    let dims = conf^.gameSettings.dimentions
    when (not (inBounds (dims, dims) x)) $ throwError OutOfBounds
    when (not ((x `notElem` xs) || conf^.gameSettings.cross)) $ throwError SelfCross

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
    (x,y) <- maybe (throwError ZeroLength) return $ listToMaybe (world ^. snake)
    
    let pos    = moves x y !! fromEnum d
    case compare (world ^. stomack) 0 of
        LT -> put $ world & snake %~ init   & stomack %~ succ -- check for init of empty list !!!
        GT -> put $ world & snake %~ (pos:) & stomack %~ pred
        EQ -> put $ world & snake %~ (\xs -> pos : init xs)
    world' <- get
    h <- maybe (throwError ZeroLength) return $ listToMaybe (world' ^. snake)
    tell $ "STEP> head: " ++ show h ++ "\n"


dirMarkov :: Game World Settings' Direction
dirMarkov = do 
    world <- get
    conf <- ask
    (x,y) <- maybe (throwError ZeroLength) return $ listToMaybe (world ^. snake)
    let ps      = world ^.. table . traverse . place
        pr      = world ^.. table . traverse . prob
            
        minPossibleIndex xs cs =    let minIndex ys = fromJust $ elemIndex (minimum ys) ys
                                        i = minIndex cs 
                                    in  if conf ^. gameSettings . cross
                                        then toEnum i
                                        else
                                            if (moves x y !! i) `notElem` xs || cs !! i == 1000000000
                                            then toEnum i
                                            else minPossibleIndex xs (cs & ix i .~ 1000000000)
    
    costs'   <- Game { unwrap = lift . lift $ magnify dynamicSettings $ markovOut (x, y) ps pr }
    
    tell $ "MARKOV OUT> costs: " ++ show costs' ++ "\n"
    
    return $ minPossibleIndex (world ^. snake) costs' 

commandMarkov :: Direction -> Game World Settings' ()
commandMarkov dir = do 
    world <- get
    p <- maybe (throwError ZeroLength) return $ listToMaybe (world ^. snake)
    put $ world & commandCounter %~ succ
    world <- get

    let pr = world ^.. table . traverse . prob
        ps = world ^.. table . traverse . place
        
    newProbs <- Game { unwrap = (lift . lift $ magnify dynamicSettings $ markovIn p ps pr (fromEnum dir)) }

    let changeProbs foods = fmap (\(a, p') -> a & prob .~ p') $ zipWith (,) foods newProbs
    
    put $ world & table . traverse . reward %~ pred 
                & table %~ changeProbs

    world' <- get

    tell $ "COMMAND> " ++ show dir ++ "; " ++ "probabilities: " ++ (show $ world' ^.. table . traverse . prob) ++ "\n"

commandSnake :: Direction -> Game World Settings' ()
commandSnake dir = do 
    world <- get
    put $ world & commandCounter %~ succ
    world <- get
    put $ commandSnake' dir $ over (table . traverse . reward) pred world
    where   commandSnake' North = over direction (\d -> if d == South then South else North)
            commandSnake' East  = over direction (\d -> if d == West  then West  else East)
            commandSnake' South = over direction (\d -> if d == North then North else South)
            commandSnake' West  = over direction (\d -> if d == East  then East  else West)
