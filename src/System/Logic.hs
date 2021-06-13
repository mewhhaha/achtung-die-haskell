{-# LANGUAGE RecordWildCards #-}

module System.Logic (system) where

import Apecs qualified
import Control.Monad.Except (throwError)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Vector.Storable qualified as Vector
import Linear (Additive (zero), Metric (distance), V2 (V2), normalize, (!*))
import Numeric.Noise (noiseValue)
import Numeric.Noise.Perlin (perlin)
import Play (Command (Reset), Tick (readRenderer, readTime), readDeltaTime)
import Player.Archetype (randomStart)
import Player.Archetype qualified as PlayerArchetype
import Player.Color qualified as PlayerColor
import Relude
import SDL qualified
import World (System', tickState)
import World.Component

inside :: Double -> V2 Double -> Bool
inside radius position = distance zero position <= radius

rotateDirection :: V2 Double -> Double -> V2 Double
rotateDirection vec v = normalize (V2 (V2 (cos v) (- sin v)) (V2 (sin v) (cos v)) !* vec)

rotationSpeed :: Double
rotationSpeed = pi

isActive :: KeyboardInput -> Bool
isActive (KeyboardInput _ KeyDown) = True
isActive (KeyboardInput _ KeyPressed) = True
isActive _ = False

isPressed :: KeyboardInput -> Bool
isPressed (KeyboardInput _ KeyPressed) = True
isPressed _ = False

endRound :: GameInput -> System' ()
endRound input = do
  renderer <- tickState readRenderer
  Apecs.set Apecs.global (CScene $ GameScene Paused input)

  (CArena _ _ (Just texture)) <- Apecs.get Apecs.global
  SDL.rendererRenderTarget renderer SDL.$= Just texture
  SDL.rendererDrawColor renderer SDL.$= minBound
  SDL.clear renderer
  Apecs.set Apecs.global $ CArena mempty (perlin 1 5 0.05 0.5) (Just texture)

  Apecs.cmapM $ \(CPlayer _) -> do
    (position, direction) <- liftIO randomStart
    return (CPosition (fromIntegral <$> position), CDirection direction, Apecs.Not :: Apecs.Not CDead)

turnPlayers :: Map Player PlayerInput -> System' ()
turnPlayers playerInputs = do
  dt <- tickState readDeltaTime
  Apecs.cmap $ \(CPlayer player, CDirection direction) ->
    let rotation s = rotateDirection direction (s * dt)
     in case Map.lookup player playerInputs of
          Just PlayerInput {turnLeft, turnRight}
            | isActive turnLeft && isActive turnRight -> Left ()
            | isActive turnLeft -> Right (CDirection $ rotation (- rotationSpeed))
            | isActive turnRight -> Right (CDirection $ rotation rotationSpeed)
          _ -> Left ()

system :: System' ()
system = do
  ct <- tickState readTime
  dt <- tickState readDeltaTime
  renderer <- tickState readRenderer
  (CScene scene) <- Apecs.get Apecs.global
  case scene of
    MenuScene input@(GameInput playerInputs _ continue) -> do
      lockedInPlayers <- Apecs.cfold (\m (CPlayer player) -> Set.insert player m) mempty

      forM_ (Map.assocs playerInputs) $ \(player, PlayerInput {..}) -> do
        when (isPressed turnLeft || isPressed turnRight) $ do
          if Set.member player lockedInPlayers
            then PlayerArchetype.remove player
            else PlayerArchetype.make player
      when (isPressed continue && Set.size lockedInPlayers >= 2) $
        Apecs.set Apecs.global $ CScene (GameScene Paused input)
    EndScene _ -> pass
    GameScene paused input@GameInput {..} -> do
      when (isActive reset) . lift $ throwError Reset

      case paused of
        Paused -> do
          n <- Apecs.cfold (\n (CPlayer _, _ :: Apecs.Not CDead) -> n + 1) (0 :: Int)

          if n < 2
            then when (isPressed continue) $ endRound input
            else when (isPressed continue) $ Apecs.set Apecs.global (CScene $ GameScene Unpaused input)
        Unpaused -> do
          turnPlayers playerInputs

          Apecs.cmap $ \(CPosition position, CSpeed speed, CDirection direction, _ :: Apecs.Not CDead) -> Just (CPosition (position + (direction * pure speed * pure dt)))

          Apecs.cmapM $ \(CPlayer player, CPosition position@(V2 px py), CBrush brush, _ :: Apecs.Not CDead, CArena arena noise target) -> do
            let space = noiseValue noise (px, py, ct)
                origin = floor <$> position
                range = subtract brush <$> [0 .. brush * 2]
                circle = fmap fromIntegral . (+ origin) <$> [p | x <- range, y <- range, let p = V2 x y, inside (fromIntegral brush) (fromIntegral <$> p)]
                covered = Map.fromList ((,player) . fmap fromIntegral <$> circle)
                color = PlayerColor.fromPlayer player
                noiseCutoff = -0.4

            if space >= noiseCutoff
              then do
                case target of
                  Nothing -> pass
                  Just texture -> do
                    SDL.rendererRenderTarget renderer SDL.$= Just texture
                    SDL.rendererDrawColor renderer SDL.$= color
                    SDL.drawPoints renderer (Vector.fromList $ SDL.P <$> circle)
                return $ Right (CArena (Map.union arena covered) noise target)
              else return $ Left ()

          Apecs.cmap $ \(CPlayer player, CPosition position@(V2 px py), CDirection direction, CBrush brush, CArena arena _ _, CScore score, _ :: Apecs.Not CDead) ->
            let tip = round <$> position + direction * pure (fromIntegral brush + 2)
                outsideArena = px < 0 || py < 0 || px > fromIntegral arenaWidth || py > fromIntegral arenaHeight
                pointAll = Map.mapWithKey (\p v -> if p /= player then v + 1 else v) score
                pointPlayer owner = Map.mapWithKey (\p v -> if p == owner then v + 1 else v) score
             in case (Map.lookup tip arena, outsideArena) of
                  (_, True) -> Right (CDead, CScore pointAll)
                  (Just owner, _)
                    | owner == player -> Right (CDead, CScore pointAll)
                    | otherwise -> Right (CDead, CScore $ pointPlayer owner)
                  _ -> Left ()

          n <- Apecs.cfold (\n (CPlayer _, _ :: Apecs.Not CDead) -> n + 1) (0 :: Int)
          when (n < 2) $ do
            Apecs.set Apecs.global (CScene $ GameScene Paused input)