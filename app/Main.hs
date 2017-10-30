{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric, DeriveAnyClass, TypeApplications, DataKinds #-}

module Main (main) where

import Graphics.Gloss.Interface.IO.Game
import System.Random
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Yaml
import Data.Maybe
import Data.Map
import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Memo hiding (fromJust, isNothing)
import Text.Parsec.Expr.Math
import Control.Lens
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
            -- f <- flip evaluate expr
            let f x y = fromJust $ evaluate (fromList [("x",x), ("y",y)]) (Just expr)
                f' (x, y) = f (fromIntegral x) (fromIntegral y)
            return $ NewSettings' 
                    { _gameSettings = cfg ^. game
                    , _snakeSettings = cfg ^. snake'
                    , _foodSettings = cfg ^. food
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

    -- let cfg = decode $ T.encodeUtf8 j :: Maybe Settings
    --     func (x, y) = do    config <- cfg
    --                         let expr = parse $ config ^. dynamic' . costString
    --                         f <- evaluate (fromList [("x",x), ("y",y)]) expr
    --                         let f' (x, y) = f (fromIntegral x) (fromIntegral y) 
    --                         f'
    --     -- func = \x y -> Data.Maybe.fromJust $ evaluate (fromList [("x",x), ("y",y)]) (Just expr)
    --     func' (x, y) = func (fromIntegral x) (fromIntegral y) 
    --     config' = NewSettings' 
    --         {
    --           _gameSettings = config ^. game
    --         , _snakeSettings = config ^. snake'
    --         , _foodSettings = config ^. food
    --         , _dynamicSettings = DynamicEnv 
    --             { _dynamicCost = func'
    --             , _dynamicLim = config ^. dynamic' . limit
    --             , _dynamicLog = config ^. dynamic' . logging
    --             , _dynamicMaxSteps = config ^. dynamic' . maxStepsSearch
    --             }
    --         }
    
    -- let config1 = eitherDecode $ T.encodeUtf8 j :: Either T.Text Settings1-- Either String Settings1
    -- 
    let gworld = 
            NewGameWorld 
            {   _snakeWorld = NewWorld
                { _direction = North
                , _snake = [config ^. snakeSettings . positionInit]
                , _stomack = config ^. snakeSettings . sizeInit
                , _isOver = False
                , _gen = mkStdGen seed
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
    let ((((_, w'), txt), cQ), cV) =  flip runMemo (w ^. cacheV) 
                                    . flip runMemoT (w ^. cacheQ) 
                                    . runWriterT 
                                    . flip runReaderT s 
                                    . flip runStateT w 
                                    . zoom gameWorld 
                                    $ f e
    when (txt /= "") $ do 
        hPutStrLn h txt
        putStrLn txt

    return $ w' & cacheQ .~ cQ & cacheV .~ cV