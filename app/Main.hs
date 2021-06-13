module Main where

import Game (initialize, step)
import Play (play)
import Relude
import qualified SDL

main :: IO ()
main =
  play
    step
    "Achtung die Haskell"
    (SDL.V2 640 480)
    initialize
