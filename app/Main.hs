{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric, DeriveAnyClass, TypeApplications, DataKinds #-}

module Main (main) where

import Graphics.Gloss.Interface.IO.Game
import System.Random
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Yaml
import Data.Maybe
import Data.Map hiding (take)
import Data.Either
import Control.Lens
import Control.Monad.State
import Control.Monad.Writer hiding ((<>))
import Control.Monad.Reader
import Control.Monad.Except
import qualified Options.Applicative as O
import Options.Applicative hiding (Parser)
import Data.Semigroup ((<>))
-- import Control.Monad.Trans.Except
import Control.Monad.Memo hiding (fromJust, isNothing)
import Text.Parsec.Expr.Math
-- import Control.Lens
import System.IO
import System.Exit
import World
import Draw
import Snake
import Dynamic

data WholeWorld = NewWholeWorld
    { _gameWorld :: GameWorld
    , _cacheQ :: Map (Int, Int, Int) [Double]
    , _cacheV :: Map (Int, Int, Int) Double
    }

makeLenses ''WholeWorld    

options :: O.Parser Opts
options = Opts
    <$> switch
        ( long "nogui"
        <> short 'n'
        <> showDefault
        <> help "do not display GUI" )
    <*> option auto
        ( long "batch"
        <> short 'b'
        <> help "run in batch mode"
        <> showDefault
        <> value 1)
    <*> switch
        ( long "verbose"
        <> short 'v'
        <> help "show log in terminal" )

main :: IO ()
main = programm =<< execParser opts
    where opts = info (options <**> helper)
            ( fullDesc
            <> progDesc "Shared Control Snake using POMD"
            <> header "The Snake" )

programm :: Opts -> IO ()   
programm op = do
    j <- T.readFile "config.yaml"
    seed <- randomIO 
    let config' = do
            cfg <- decode $ T.encodeUtf8 j :: Maybe Settings
            expr <- case parse $ cfg ^. dynamic' . costString of 
                        Left _ -> Nothing
                        Right e -> Just e
            let f x y = fromJust $ evaluate (fromList [("x",x), ("y",y)]) (Just expr)
                f' (x, y) = f (fromIntegral x) (fromIntegral y)
            return $ NewSettings' 
                    { _gameSettings = cfg ^. game
                    , _snakeSettings = cfg ^. snake'
                    , _foodSettings = cfg ^. food
                    , _playerSettings = cfg ^. player
                    , _dynamicSettings = DynamicEnv 
                        { _dynamicCost = f'
                        , _dynamicLim = cfg ^. dynamic' . limit
                        , _dynamicLog = cfg ^. dynamic' . logging
                        , _dynamicMaxSteps = cfg ^. dynamic' . maxStepsSearch
                        }
                    , _opts = op
                    }
    when (isNothing config') $ do
            putStrLn "Error parsing config"
            exitWith (ExitFailure 1)
    let config = fromJust config'

    -- 
    let gworld = 
            NewGameWorld 
            {   _snakeWorld = NewWorld
                { _direction = North
                , _snake = [config ^. snakeSettings . positionInit]
                , _stomack = config ^. snakeSettings . sizeInit
                , _gen = mkStdGen seed
                , _visited = []
                , _tick = 0
                , _stepCounter = 0
                , _commandCounter = 0
                , _goodFoodCounter = 0
                , _badFoodCounter = 0
                , _table =  let foods = config ^. foodSettings . exact 
                                foodPlace p x r = NewFood {_place = x, _reward = r, _prob = p}
                            in case foods ^. positions of
                                []  -> [NewFood {_place = (0, 1), _reward = 0, _prob = 1}]
                                _   -> zipWith  (foodPlace ((1 /) . fromIntegral . length $ foods ^. positions))
                                                (foods ^. positions)
                                                (foods ^. costs) } 
            ,   _resolution = config ^. gameSettings . pixels }

        wholeWorld = NewWholeWorld { _gameWorld = gworld, _cacheQ = Empty, _cacheV = Empty }

        play' = playIO    
                    (displayMode $ wholeWorld ^. gameWorld)
                    backgroundColor
                    (config ^. gameSettings . rate)
                    wholeWorld
                    (return . drawWorld (config ^. gameSettings . dimentions) . flip (^.) gameWorld)

    outh <- openFile "output.txt" WriteMode

    if config^.opts.nogui
    then do
            when (not $ config^.gameSettings.autoSteps) $ 
                exitMessage outh "autoSteps should be true for non gui mode (no user input should be required to initiate each step)"
            when (config^.gameSettings.direct) $ 
                exitMessage outh "direct should be False for non gui mode (no direct control from user possible)"
            when (not $ config^.playerSettings.autoPlay) $
                exitMessage outh "autoPlay should be true ti run in non GUI mode (programm decides on control input)"        
            let singleRun seed = do 
                    let stepSnake' = do stepSnake
                                        return NoError
                        simulate   = do e <- stepSnake' `catchError` \e -> (return e)
                                        case e of 
                                            NoError -> simulate
                                            _       -> throwError e
                        ((((err, w), txt), _), _) = retrieve (wholeWorld & gameWorld.snakeWorld.gen .~ mkStdGen seed) config (gameWorld.snakeWorld) simulate

                    when (config^.opts.verbose) $ putStrLn txt 
                    when (isLeft err) $ output err $ w^.gameWorld.snakeWorld
                    return (err, w)

            seeds <- replicateM (config^.opts.batch) randomIO 
            results <- mapM singleRun seeds
            putStrLn $ take 40 . repeat $ '*'
            putStrLn $ "SUMMARY"
            putStrLn $ take 40 . repeat $ '*'            
            putStrLn $ show $ fromListWith (+) [(x, 1) | x <- lefts (results^..traverse._1)]
            let ws = results^..traverse._2
                mean xs = fromIntegral (sum xs) / fromIntegral (length xs)
            putStrLn $ "Total steps MEAN: "     ++ show (mean $ ws^..traverse.gameWorld.snakeWorld.stepCounter)
            putStrLn $ "Commands issued MEAN: " ++ show (mean $ ws^..traverse.gameWorld.snakeWorld.commandCounter)
            putStrLn $ "Good Food eaten MEAN: " ++ show (mean $ ws^..traverse.gameWorld.snakeWorld.goodFoodCounter)
            putStrLn $ "Bad Food eaten MEAN: "  ++ show (mean $ ws^..traverse.gameWorld.snakeWorld.badFoodCounter)
            putStrLn $ "Total length MEAN: "    ++ show (mean $ length <$> ws^..traverse.gameWorld.snakeWorld.snake) ++ "\n"
        
    else do
        when (config^.opts.batch /= 1) $
            do  putStrLn "batch is only possible for nogui mode"
                hClose outh
                System.Exit.exitSuccess                

        play' (handlerE config outh handleEvent)
              (handler  config outh handleStep)

    hClose outh

handlerE :: Settings' -> Handle -> (Event -> Game GameWorld Settings' ()) -> Event -> WholeWorld -> IO WholeWorld
handlerE s h f e w = do
    case e of 
        EventKey key state' _ _ -> case state' of
            Down -> case key of
                SpecialKey KeyEsc -> do 
                    hClose h
                    System.Exit.exitSuccess
                _ -> handler s h f e w
            _ -> handler s h f e w
        _ -> handler s h f e w


handler :: Settings' -> Handle -> (e -> Game GameWorld Settings' ()) -> e -> WholeWorld -> IO WholeWorld
handler s h f e w = do
    let ((((err, w'), txt), cQ), cV) = retrieve w s gameWorld (f e)

    when (isLeft err) $ do
        output err $ w'^.gameWorld.snakeWorld
        hClose h
        System.Exit.exitSuccess

    when (txt /= "") $ do 
        hPutStrLn h txt
        when (s^.opts.verbose) $ putStrLn txt

    return $ w' & cacheQ .~ cQ & cacheV .~ cV
    where 

output err w = do
    putStrLn $ take 40 . repeat $ '*'
    putStrLn $ "GAMEOVER due to "   ++ show ((\(Left err') -> err') err)
    putStrLn $ take 40 . repeat $ '*'    
    putStrLn $ "Total steps: "      ++ show (w^.stepCounter)
    putStrLn $ "Commands issued: "  ++ show (w^.commandCounter)
    putStrLn $ "Good Food eaten: "  ++ show (w^.goodFoodCounter)
    putStrLn $ "Bad Food eaten: "   ++ show (w^.badFoodCounter)
    putStrLn $ "Total length: "     ++ show (length $ w^.snake) ++ "\n"
        
retrieve w s z g = flip runMemo (w^.cacheV) 
    . flip runMemoT (w^.cacheQ) 
    . runWriterT 
    . flip runReaderT s 
    . flip runStateT w 
    . zoom z 
    . runExceptT 
    . unwrap $ g

exitMessage outh msg = do 
    putStrLn msg
    hClose outh
    System.Exit.exitSuccess