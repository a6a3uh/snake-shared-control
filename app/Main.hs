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
import System.IO
import System.Exit
import World
import Draw
                        
main :: IO ()
main = do
    j <- T.readFile "config.yaml"
    seed <- randomIO 
    let config = fromJust (decode $ T.encodeUtf8 j :: Maybe Settings) 
    
    -- let config1 = eitherDecode $ T.encodeUtf8 j :: Either T.Text Settings1-- Either String Settings1
    -- 
    let gworld = 
            NewGameWorld 
            {   _snakeWorld = NewWorld
                { _direction = North
                , _snake = [config ^. snake' . position]
                , _stomack = config ^. snake' . size
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

    let play' = playIO    
                    (displayMode gworld)
                    backgroundColor
                    (config ^. game . rate)
                    gworld
                    (return . drawWorld (config ^. game . dimentions . _1))

    outh <- openFile "output.txt" WriteMode
                    
    play'   (handlerE config outh handleEvent)
            (handler  config outh handleStep)

    hClose outh

handlerE :: Settings -> Handle -> (Event -> Game GameWorld Settings Log ()) -> Event -> GameWorld -> IO GameWorld
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

handler :: Settings -> Handle -> (e -> Game GameWorld Settings Log ()) -> e -> GameWorld -> IO GameWorld
handler s h f e w = do
    let ((_, w'), txt) = runWriter $ (flip runReaderT s) $ (runStateT (f e) w)
    case txt of
        [] -> return ()
        _  -> do hPutStrLn h txt
                 putStrLn txt
    return w'