module Main where

import Game (initialize, step)
import Play (play)
import Relude
import qualified SDL

main :: IO ()
main =
  play
    step
    "Apecs snake"
    (SDL.V2 640 480)
    initialize
