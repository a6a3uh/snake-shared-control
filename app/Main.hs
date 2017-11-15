{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric, DeriveAnyClass, TypeApplications, DataKinds #-}

module Main (main) where

import Graphics.Gloss.Interface.IO.Game
import System.Random
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Yaml
import Data.Maybe
import Data.Map
import Data.Either
import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
-- import Control.Monad.Trans.Except
import Control.Monad.Memo hiding (fromJust, isNothing)
import Text.Parsec.Expr.Math
-- import Control.Lens
import System.IO
import System.Exit
import World
import Draw
import Dynamic



data WholeWorld = NewWholeWorld
    { _gameWorld :: GameWorld
    , _cacheQ :: Map (Int, Int, Int) [Double]
    , _cacheV :: Map (Int, Int, Int) Double
    }

makeLenses ''WholeWorld         

main :: IO ()
main = do
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
                    
    play'   (handlerE config outh handleEvent)
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
    let ((((err, w'), txt), cQ), cV) =    flip runMemo (w ^. cacheV) 
                                        . flip runMemoT (w ^. cacheQ) 
                                        . runWriterT 
                                        . flip runReaderT s 
                                        . flip runStateT w 
                                        . zoom gameWorld 
                                        . runExceptT 
                                        . unwrap
                                        $ f e -- `catchError` (const $ tell "GAMEOVER")

    when (isLeft err) $ do
        putStrLn $ "GAMEOVER due to "   ++ show ((\(Left err') -> err') err)
        putStrLn $ "Total steps: "      ++ show (w'^.gameWorld.snakeWorld.stepCounter)
        putStrLn $ "Commands issued: "  ++ show (w'^.gameWorld.snakeWorld.commandCounter)
        putStrLn $ "Good Food eaten: "  ++ show (w'^.gameWorld.snakeWorld.goodFoodCounter)
        putStrLn $ "Bad Food eaten: "   ++ show (w'^.gameWorld.snakeWorld.badFoodCounter)
        putStrLn $ "Total length: "     ++ show (length $ w'^.gameWorld.snakeWorld.snake)
        
        hClose h
        System.Exit.exitSuccess

    when (txt /= "") $ do 
        hPutStrLn h txt
        when (s^.gameSettings.log') $ putStrLn txt

    return $ w' & cacheQ .~ cQ & cacheV .~ cV
    where 