module Reload where

import qualified Control.Concurrent.Async as Async
import Data.Text ()
import Game (initialize, step)
import Play (play)
import qualified Rapid
import Relude
import qualified SDL

update :: IO ()
update = do
  Rapid.rapid 0 $ \r -> do
    var <- Rapid.createRef @Text r "step"  (newTMVarIO step)

    Rapid.startWith Async.asyncBound r "sdl" $ do
      let updateStep = do
            join . liftIO . atomically $ readTMVar var
      play
        updateStep
        "Apecs snake"
        (SDL.V2 640 480)
        initialize

    _ <- atomically $ swapTMVar var step
    pure ()
