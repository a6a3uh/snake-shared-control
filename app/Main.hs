{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric, DeriveAnyClass, TypeApplications, DataKinds #-}

module Main (main) where

import Graphics.Gloss.Interface.IO.Game
import System.Random
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Yaml
import Data.Maybe
import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Memo
import Control.Lens
import System.IO
import System.Exit
import World
import Draw
import Dynamic

data WholeWorld = NewWholeWorld
    { _gameWorld :: GameWorld
    , _stub :: Int
    }

makeLenses ''WholeWorld         

main :: IO ()
main = do
    j <- T.readFile "config.yaml"
    seed <- randomIO 
    let config = Data.Maybe.fromJust (decode $ T.encodeUtf8 j :: Maybe Settings) 
        config' = NewSettings' {
              _gameSettings = config ^. game
            , _snakeSettings = config ^. snake'
            , _foodSettings = config ^. food
            , _dynamicSettings = DynamicEnv {_dynamicCost = costLogistic, _dynamicLim = 10, _dynamicLog = False, _dynamicMaxSteps = 5}
        }
    
    -- let config1 = eitherDecode $ T.encodeUtf8 j :: Either T.Text Settings1-- Either String Settings1
    -- 
    let gworld = 
            NewGameWorld 
            {   _snakeWorld = NewWorld
                { _direction = North
                , _snake = [config ^. snake' . positionInit]
                , _stomack = config ^. snake' . sizeInit
                , _isOver = False
                , _gen = mkStdGen seed
                , _table =  let foods = config ^. food . exact 
                                foodPlace p x r = NewFood {_place = x, _reward = r, _prob = p}
                            in case foods ^. positions of
                                []  -> [NewFood {_place = (0, 1), _reward = 0, _prob = 1}]
                                _   -> zipWith  (foodPlace ((1 /) . fromIntegral . length $ foods ^. positions))
                                                (foods ^. positions)
                                                (foods ^. costs) } 
            ,   _resolution = config ^. game . pixels }

        wholeWorld = NewWholeWorld { _gameWorld = gworld, _stub = 0 }

        play' = playIO    
                    (displayMode $ wholeWorld ^. gameWorld)
                    backgroundColor
                    (config ^. game . rate)
                    wholeWorld
                    (return . drawWorld (config ^. game . dimentions . _1) . flip (^.) gameWorld)

    outh <- openFile "output.txt" WriteMode
                    
    play'   (handlerE config' outh handleEvent)
            (handler  config' outh handleStep)

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

    -- if e ^? _Ctor @"EventKey" . getField @"Key" . _Ctor @"SpecialKey" == Just KeyEsc
    -- then System.Exit.exitSuccess
    -- else
    --      handler h f e w

handler :: Settings' -> Handle -> (e -> Game GameWorld Settings' ()) -> e -> WholeWorld -> IO WholeWorld
handler s h f e w = do
    let ((((_, w'), txt),_),_) =  startRunMemo . startRunMemoT . runWriterT . flip runReaderT s . flip runStateT w $ zoom gameWorld $ f e
    case txt of
        [] -> return ()
        _  -> do hPutStrLn h txt
                 putStrLn txt
    return w'