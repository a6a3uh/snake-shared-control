{-# LANGUAGE TemplateHaskell, Rank2Types #-}

module World where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import System.Random
import Control.Lens
import Linear.V2
-- import qualified Data.MemoCombinators as Memo

data Settings = NewSettings
    { numFoodBounds :: (Int, Int)
    , rewardBounds :: (Int, Int)
    , worldScale :: Int
    }

type Pos = V2 Int

data Direction
    = North
    | East
    | South
    | West
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
type MotionCost = [Int]

data Food = NewFood
    { _place :: Pos
    , _reward :: Reward
    } deriving (Read, Show)

makeLenses ''World
makeLenses ''Food

instance Arbitrary Food where
  arbitrary = do
    let s = (worldScale settings `div` 2) - 1
    x <- choose (-s, s)
    y <- choose (-s, s)
    rew <- choose (rewardBounds settings)
    return $ NewFood (V2 x y) rew

settings :: Settings
settings = NewSettings
    { numFoodBounds = (2, 5)
    , rewardBounds = (0, 5)
    , worldScale = 15
    }

initialWorld :: Int -> World
initialWorld seed = NewWorld
    { _direction = North
    , _snake = [V2 0 2, V2 0 1, V2 0 0, V2 0 (-1), V2 0 (-2)]
    , _stomack = 0
    , _isOver = False
    , _gen = mkStdGen seed
    , _table = [NewFood {_place = V2 0 3, _reward = 0}]
    }

inBounds :: Pos -> Bool
inBounds (V2 x y) =
    let s = worldScale settings `div` 2
    in  -s <= x && x <= s && -s <= y && y <= s
