{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric, DeriveAnyClass, TypeApplications, DataKinds #-}

module World where

import System.Random
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import GHC.Generics
import Data.Aeson.Types

data CostSettings = NewCostSettings
    { _function :: String
    } deriving  ( Generic
                , Show)

data GameSettings = NewGameSettings
    { _auto :: Bool
    , _direct :: Bool
    , _cross :: Bool
    , _steps :: Int
    , _rate :: Int
    , _dimentions :: (Int, Int)
    , _pixels :: (Int, Int)
    , _cost :: CostSettings
    } deriving  ( Generic
                , Show)

data SnakeSettings = NewSnakeSettings
    { _size :: Int
    , _position :: (Int, Int)
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
    } deriving  ( Generic
                , Show)

makeLenses ''CostSettings
makeLenses ''GameSettings
makeLenses ''SnakeSettings
makeLenses ''FoodSettings
makeLenses ''FoodData
makeLenses ''Settings

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

type Game g r w = StateT g (ReaderT r (Writer w))
type Log = String

type Pos = (Int, Int)

data Direction
    = West
    | East
    | South
    | North  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data World = NewWorld
    { _direction :: Direction
    , _snake :: [Pos]
    , _stomack :: Reward
    , _isOver :: Bool
    , _gen :: StdGen
    , _table :: [Food]
    } deriving (Read, Show)

type Reward = Int
type Probability = Double

data Food = NewFood
    { _place :: Pos
    , _reward :: Reward
    , _prob :: Probability
    } deriving (Read, Show)

makeLenses ''World
makeLenses ''Food