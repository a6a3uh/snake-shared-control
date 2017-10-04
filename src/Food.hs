module Food (moveFood, eatFood) where

import System.Random
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Random
import Test.QuickCheck.Gen
import Data.List.Unique
import Control.Lens
import Data.Maybe
import Control.Monad.State
import Control.Monad.Writer

import World

moveFood :: StateT World (Writer String) ()
moveFood = do
  world <- get
  let (seed, g) = random (world ^. gen)
  let newTable = unGen arbitrary (mkQCGen seed) (snd . numFoodBounds $ settings)
  let places = newTable ^.. traverse . place
  if  (length newTable < (fst . numFoodBounds) settings)    -- num of foods too small
    || length (unique places) /= length places              -- several foods at same place
  then do put $ world & gen .~ g
          moveFood
  else let newTable' = newTable & traverse . prob %~ ( / (fromIntegral . length) newTable )
       in put $ world & gen .~ g & table .~ newTable'

eatFood :: StateT World (Writer String) Bool
eatFood  = do
  world <- get
  let rew = world ^? table . traverse . filtered (match world) . reward
  put $ world & stomack %~ (+ fromMaybe 0 rew)
  return $ isJust rew
  where match w fd = fd ^. place == head (w ^. snake)
