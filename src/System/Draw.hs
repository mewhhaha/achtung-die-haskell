module System.Draw (system) where

import Apecs qualified
import Data.Vector.Storable qualified as Vector
import Linear (Additive (zero), Metric (distance), V2 (..))
import Play (readRenderer)
import Player.Color qualified as PlayerColor
import Relude
import SDL qualified
import World (System', tickState)
import World.Component

inside :: Double -> V2 Double -> Bool
inside radius position = distance zero position <= radius

system :: System' ()
system = do
  (CArena _ _ target, CScene scene) <- Apecs.get Apecs.global
  case scene of
    GameScene {} -> do
      renderer <- tickState readRenderer
      SDL.rendererRenderTarget renderer SDL.$= Nothing

      case target of
        Nothing -> pass
        Just texture -> do
          SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (SDL.P $ V2 0 0) (fromIntegral <$> V2 arenaWidth arenaHeight))

      Apecs.cmapM_ $ \(CPlayer player, CPosition position, CBrush brush) -> do
        let origin = floor <$> position
            range = subtract brush <$> [0 .. brush * 2]
            circle = fmap fromIntegral . (+ origin) <$> [p | x <- range, y <- range, let p = V2 x y, inside (fromIntegral brush) (fromIntegral <$> p)]
            color = PlayerColor.fromPlayer player

        SDL.rendererDrawColor renderer SDL.$= color
        SDL.drawPoints renderer (Vector.fromList $ SDL.P <$> circle)
    _ -> pass