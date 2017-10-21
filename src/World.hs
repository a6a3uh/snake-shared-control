{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric, DeriveAnyClass, TypeApplications, DataKinds #-}

module World where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import System.Random
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import GHC.Generics
import Data.Aeson.Types

data CostSettings = NewCostSettings
    { _function :: String
    , _scale1 :: Double 
    } deriving  ( Generic
                , Show)

data GameSettings1 = NewGameSettings1
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
    { _size1 :: Int
    , _position :: (Int, Int)
    } deriving  ( Generic
                , Show)

data FoodSettings = NewFoodSettings
    { _number :: (Int, Int)
    , _reward1 :: (Int, Int)
    , _exact :: FoodData
    } deriving  ( Generic
                , Show)

data FoodData = NewFoodData
    { _positions :: [(Int, Int)]
    , _costs :: [Int]
    } deriving  ( Generic
                , Show)

data Settings1 = NewSettings1
    { _game :: GameSettings1
    , _snake1 :: SnakeSettings
    , _food :: FoodSettings
    } deriving  ( Generic
                , Show)

makeLenses ''CostSettings
makeLenses ''GameSettings1
makeLenses ''SnakeSettings
makeLenses ''FoodSettings
makeLenses ''FoodData
makeLenses ''Settings1

instance FromJSON CostSettings where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = \s -> if s == "_scale1" then  "scale" else drop 1 s }

instance FromJSON Settings1 where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = \s -> if s == "_snake1" then  "snake" else drop 1 s }

instance FromJSON FoodData where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1}

instance FromJSON FoodSettings where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = \s -> if s == "_reward1" then "reward" else drop 1 s }

instance FromJSON SnakeSettings where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = \s -> if s == "_size1" then "size" else drop 1 s }

instance FromJSON GameSettings1 where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1}

type Game g r w = StateT g (ReaderT r (Writer w))
type Log = String

data Settings = NewSettings
    { numFoodBounds :: (Int, Int)
    , rewardBounds :: (Int, Int)
    , worldScale :: Int
    }

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

instance Arbitrary Food where
  arbitrary = do
    let s = (worldScale settings `div` 2) - 1
    x <- choose (-s, s)
    y <- choose (-s, s)
    rew <- choose (rewardBounds settings)
    let p  = 1
    return $ NewFood (x, y) rew p

settings :: Settings
settings = NewSettings
    { numFoodBounds = (2, 5)
    , rewardBounds = (-5, 5)
    , worldScale = 15
    }

inBounds :: Pos -> Bool
inBounds (x, y) =
    let s = worldScale settings `div` 2
    in  -s <= x && x <= s && -s <= y && y <= s
