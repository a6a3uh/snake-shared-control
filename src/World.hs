{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric, DeriveAnyClass, TypeApplications, DataKinds #-}

module World where

import System.Random
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import GHC.Generics
import Data.Aeson.Types
import Dynamic

data CostSettings = NewCostSettings
    { _function :: String
    } deriving  ( Generic
                , Show)

data GameSettings = NewGameSettings
    { _auto :: Bool
    , _direct :: Bool
    , _cross :: Bool
    , _rate :: Int
    , _dimentions :: (Int, Int)
    , _pixels :: (Int, Int)
    } deriving  ( Generic
                , Show)

data SnakeSettings = NewSnakeSettings
    { _sizeInit :: Int
    , _positionInit :: (Int, Int)
    } deriving  ( Generic
                , Show)

data FoodSettings = NewFoodSettings
    { _number :: (Int, Int)
    , _rewards :: (Int, Int)
    , _exact :: FoodData
    } deriving  ( Generic
                , Show)

data FoodData = NewFoodData
    { _positions :: [(Int, Int)]
    , _costs :: [Int]
    } deriving  ( Generic
                , Show)

data Settings = NewSettings
    { _game :: GameSettings
    , _snake' :: SnakeSettings
    , _food :: FoodSettings
    -- , _dynamic :: DynamicEnv
    } deriving  ( Generic
                , Show)

data Settings' = NewSettings'
    { _gameSettings :: GameSettings
    , _snakeSettings :: SnakeSettings
    , _foodSettings :: FoodSettings
    , _dynamicSettings :: DynamicEnv Int Double
    } deriving  ( Generic )           

makeLenses ''CostSettings
makeLenses ''GameSettings
makeLenses ''SnakeSettings
makeLenses ''FoodSettings
makeLenses ''FoodData
makeLenses ''Settings
makeLenses ''Settings'

instance FromJSON CostSettings where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 1}

instance FromJSON Settings where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = \s -> if s == "_snake'" then  "snake" else drop 1 s }

instance FromJSON FoodData where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1}

instance FromJSON FoodSettings where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 1}

instance FromJSON SnakeSettings where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 1}        

instance FromJSON GameSettings where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1}

type Game s r = StateT s (ReaderT r (WriterT Log (MemoQV Int Double)))
type Log = String

dynamicEnv = DynamicEnv {_dynamicCost = costLogistic, _dynamicLim = 10, _dynamicLog = False, _dynamicMaxSteps = 5}

-- type Pos = (Int, Int)

data Direction
    = West
    | East
    | South
    | North  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data World = NewWorld
    { _direction :: Direction
    , _snake :: [Pos Int]
    , _stomack :: Reward
    , _isOver :: Bool
    , _gen :: StdGen
    , _table :: [Food]
    } deriving (Read, Show)

type Reward = Int
type Probability = Double

data Food = NewFood
    { _place :: Pos Int
    , _reward :: Reward
    , _prob :: Probability
    } deriving (Read, Show)

makeLenses ''World
makeLenses ''Food