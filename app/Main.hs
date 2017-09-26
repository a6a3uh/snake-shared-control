module Main (main) where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Draw

main :: IO ()
main = do
    seed <- randomIO
    let gworld = initialGameWorld seed

    play
        (displayMode gworld)
        backgroundColor
        (stepRate gameSettings)
        gworld
        drawWorld
        handleEvent
        handleGameStep
