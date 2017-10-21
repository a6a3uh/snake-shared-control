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
-- import System.Random
import Control.Monad.Random

import World

randomTable :: (RandomGen g) => (Int, Int) -> (Int, Int) -> (Int, Int) -> Rand g [Food]
randomTable dims rews nums = do
  let rewardFoods = getRandomR rews
      pos n = let n' = n `div` 2 in getRandomR (-n', n')
      posFoods = liftM2 (,) (pos $ fst dims) (pos $ snd dims)  
  numderFoods <- getRandomR nums
  rewards <- sequence $ replicate numderFoods rewardFoods
  places <- sequence $ replicate numderFoods posFoods
  if length (unique places) /= length places
  then randomTable dims rews nums
  else return $ (zipWith NewFood places rewards) <*> [1 / fromIntegral numderFoods]

moveFood :: Game World Settings Log ()
moveFood = do
  world <- get
  conf <- ask
  let dims = conf ^. game . dimentions
      rews = conf ^. food . rewards
      nums = conf ^. food . number
      (newTable, g) = runRand (randomTable dims rews nums) (world ^. gen) 
  
  tell $ "FOOD> new positions: " ++ (show $ newTable ^.. traverse. place) ++ "\n"
  put $ world & gen .~ g & table .~ newTable

eatFood :: Game World Settings Log Bool
eatFood  = do
  conf <- ask
  world <- get
  let rew = world ^? table . traverse . filtered (match world) . reward
  modify (& stomack %~ (+ fromMaybe 0 rew))
  if (isJust rew) 
  then do
    tell $ "FOOD> eaten!\n"
    if conf ^. food . exact . positions == []
    then return True
    else do
      put $ world & isOver .~ True
      return True
  else return False
  where match w fd = fd ^. place == head (w ^. snake)
