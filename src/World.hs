{-# LANGUAGE TemplateHaskell, Rank2Types #-}

module World where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import System.Random
import Control.Lens
-- import qualified Data.MemoCombinators as Memo

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
    | North
    
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

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

initialWorld :: Int -> World
initialWorld seed = NewWorld
    { _direction = North
    , _snake = [(0, 2), (0, 1), (0, 0), (0, (-1)), (0, (-2))]
    , _stomack = 0
    , _isOver = False
    , _gen = mkStdGen seed
    , _table = [NewFood {_place = (0, 3), _reward = 0, _prob = 1}]
    }

inBounds :: Pos -> Bool
inBounds (x, y) =
    let s = worldScale settings `div` 2
    in  -s <= x && x <= s && -s <= y && y <= s
