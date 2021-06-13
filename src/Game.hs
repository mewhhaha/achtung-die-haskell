module Game
  ( step,
    initialize,
  )
where

import Apecs
import Play
import Relude
import SDL qualified
import System.Draw qualified as Draw
import System.Input qualified as Input
import System.Logic qualified as Logic
import System.Overlay qualified as Overlay
import System.Setup qualified as Setup
import World

step :: Step IO World
step = do
  world <- asks readWorld
  Apecs.runWith world (Input.system >> Logic.system >> Draw.system >> Overlay.system >> ask)

initialize :: SDL.Window -> SDL.Renderer -> IO World
initialize window renderer = do
  w <- initWorld
  Apecs.runWith w (Setup.system window renderer >> ask)