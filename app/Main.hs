{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main (main) where

import Graphics.Gloss.Interface.Pure.Game
import System.Random
-- import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Yaml
import Data.Aeson.Types
-- import Data.Aeson.Lens
-- import Data.Aeson
import Data.Maybe
import Control.Lens
import GHC.Generics
import World

import Draw

data CostSettings = NewCostSettings
    { _function :: String
    , _scale :: Double 
    } deriving (Generic, Show)

data GameSettings1 = NewGameSettings1
    { _auto :: Bool
    , _direct :: Bool
    , _cross :: Bool
    , _steps :: Int
    , _rate :: Int
    , _dimentions :: (Int, Int)
    , _pixels :: (Int, Int)
    , _cost :: CostSettings
    } deriving (Generic, Show)

data SnakeSettings = NewSnakeSettings
    { _size1 :: Int
    , _position :: (Int, Int)
    } deriving (Generic, Show)

data FoodSettings = NewFoodSettings
    { _number :: (Int, Int)
    , _reward1 :: (Int, Int)
    , _exact :: FoodData
    } deriving (Generic, Show)

data FoodData = NewFoodData
    { _positions :: [(Int, Int)]
    , _costs :: [Int]
    } deriving (Generic, Show)

data Settings1 = NewSettings1
    { _game :: GameSettings1
    , _snake1 :: SnakeSettings
    , _food :: FoodSettings
    } deriving (Generic, Show)

makeLenses ''CostSettings
makeLenses ''GameSettings1
makeLenses ''SnakeSettings
makeLenses ''FoodSettings
makeLenses ''FoodData
makeLenses ''Settings1

instance FromJSON CostSettings where
    parseJSON = genericParseJSON defaultOptions {
                    fieldLabelModifier = drop 1 }

instance FromJSON Settings1 where
    parseJSON = genericParseJSON defaultOptions {
                    fieldLabelModifier = drop 1 }

instance FromJSON FoodData where
    parseJSON = genericParseJSON defaultOptions {
                    fieldLabelModifier = drop 1 }
instance FromJSON FoodSettings where
    parseJSON = genericParseJSON defaultOptions {
                    fieldLabelModifier = drop 1 }

instance FromJSON SnakeSettings where
    parseJSON = genericParseJSON defaultOptions {
                    fieldLabelModifier = drop 1 }
instance FromJSON GameSettings1 where
    parseJSON = genericParseJSON defaultOptions {
                    fieldLabelModifier = drop 1 }

                        
main :: IO ()
main = do
    j <- T.readFile "config.yaml"
    seed <- randomIO 
    let config = fromJust (decode $ T.encodeUtf8 j :: Maybe Settings1) 
    
    -- let config1 = eitherDecode $ T.encodeUtf8 j :: Either T.Text Settings1-- Either String Settings1
    
    let gworld = 
            NewGameWorld {
                _snakeWorld = NewWorld
                { _direction = North
                , _snake = [(0, 0)]
                , _stomack = config ^. snake1 . size1
                , _isOver = False
                , _gen = mkStdGen seed
                , _table =  let foods = config ^. food . exact 
                                foodPlace p x r = NewFood {_place = x, _reward = r, _prob = p}
                            in case foods ^. positions of
                                []        -> [NewFood {_place = (0, 1), _reward = 0, _prob = 1}]
                                otherwise -> zipWith (foodPlace ((1 /) . fromIntegral . length $ foods ^. positions))
                                                     (foods ^. positions)
                                                     (foods ^. costs)
                } 
            , _resolution = config ^. game . pixels
        }
        -- -- = initialGameWorld seed

    play
        (displayMode gworld)
        backgroundColor
        (config ^. game . rate)
        gworld
        drawWorld
        handleEvent
        handleGameStep
