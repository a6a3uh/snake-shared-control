{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- {-# LANGUAGE DerivingStrategies #-}

module World where

import System.Random
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
-- import Control.Monad.Trans.Either
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
    , _dimentions :: Int
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

data PlayerSettings = NewPlayerSettings
    { _autoPlay :: Bool
    , _commandOnStep :: Int
    } deriving  ( Generic
                , Show)

data DynamicSettings = NewDynamicSettings
    { _costString :: String
    , _limit :: Int
    , _logging :: Bool
    , _maxStepsSearch :: Int
    } deriving  ( Generic
                , Show)

data Settings = NewSettings
    { _game :: GameSettings
    , _snake' :: SnakeSettings
    , _food :: FoodSettings
    , _dynamic' :: DynamicSettings
    , _player :: PlayerSettings 
    } deriving  ( Generic
                , Show)

data Settings' = NewSettings'
    { _gameSettings :: GameSettings
    , _snakeSettings :: SnakeSettings
    , _foodSettings :: FoodSettings
    , _dynamicSettings :: DynamicEnv Int Double
    , _playerSettings :: PlayerSettings
    } deriving  ( Generic )           

makeLenses ''CostSettings
makeLenses ''GameSettings
makeLenses ''SnakeSettings
makeLenses ''FoodSettings
makeLenses ''FoodData
makeLenses ''PlayerSettings
makeLenses ''DynamicSettings
makeLenses ''Settings
makeLenses ''Settings'

instance FromJSON CostSettings where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 1}

instance FromJSON Settings where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = \s -> case s of 
                    "_snake'"   ->  "snake" 
                    "_dynamic'" -> "dynamic"
                    _           -> drop 1 s }

instance FromJSON FoodData where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1}

instance FromJSON DynamicSettings where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 1}

instance FromJSON PlayerSettings where
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

newtype Game s r a = 
    Game { unwrap :: (ExceptT Error (StateT s (ReaderT r (WriterT Log (MemoQV Int Double)))) a) }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState s
             , MonadReader r
             , MonadWriter Log )

-- deriving instance Zoom (Game s r) (Game t r) s t

-- instance Zoom (Game s r) (Game t r) s t where
    -- zoom l = Game . zoom l . unwrap

type Log = String
type Error = ()

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
    , _tick :: Int
    , _visited :: [Pos Int]
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