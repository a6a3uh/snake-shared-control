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
import Control.Monad.Reader

import World

moveFood :: Game World a Log ()
moveFood = do
  world <- get
  let (seed, g) = random (world ^. gen)
      newTable = unGen arbitrary (mkQCGen seed) (snd . numFoodBounds $ settings)
      places = newTable ^.. traverse . place
  
  if  (length newTable < (fst . numFoodBounds) settings)    -- num of foods too small
    || length (unique places) /= length places              -- several foods at same place
  then do modify (& gen .~ g)
          moveFood
  else do let newTable' = newTable & traverse . prob %~ ( / (fromIntegral . length) newTable )
          tell $ "FOOD> new positions: " ++ (show $ newTable' ^.. traverse. place) ++ "\n"
          put $ world & gen .~ g & table .~ newTable'


eatFood :: Game World a Log Bool
eatFood  = do
  world <- get
  let rew = world ^? table . traverse . filtered (match world) . reward
  modify (& stomack %~ (+ fromMaybe 0 rew))
  if (isJust rew) 
  then do
    tell $ "FOOD> eaten!\n"
    return True
  else return False
  where match w fd = fd ^. place == head (w ^. snake)
