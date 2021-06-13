module Player.Archetype where

import Apecs qualified
import Data.Map qualified as Map
import Linear (V2 (..), normalize)
import Relude
  ( Double,
    Either (Left, Right),
    Eq ((==)),
    IO,
    Int,
    Monad (return),
    MonadIO (liftIO),
    Num ((+), (-)),
    fromIntegral,
    pass,
    ($),
    (<$>),
  )
import System.Random (randomRIO)
import World (System')
import World.Component
  ( CBrush (..),
    CDirection (..),
    CPlayer (..),
    CPosition (..),
    CScore (..),
    CSpeed (..),
    Player,
    arenaHeight,
    arenaWidth,
  )

padding :: Int -> (Int, Int) -> (Int, Int)
padding p (x, y) = (x + p, y - p)

randomStart :: IO (V2 Int, V2 Double)
randomStart = do
  px <- randomRIO (padding 50 (0, arenaWidth))
  py <- randomRIO (padding 50 (0, arenaHeight))
  vx <- randomRIO (-1, 1)
  vy <- randomRIO (-1, 1)
  return (V2 px py, normalize $ V2 vx vy)

remove :: Player -> System' ()
remove removed = do
  Apecs.cmap $ \(CPlayer player) ->
    if player == removed
      then Right (Apecs.Not :: Apecs.Not (CPlayer, CPosition, CDirection, CBrush, CSpeed))
      else Left ()

  Apecs.modify Apecs.global $ \(CScore score) -> CScore $ Map.delete removed score

make :: Player -> System' ()
make player = do
  (position, direction) <- liftIO randomStart
  _ <- Apecs.newEntity (CPlayer player, CPosition (fromIntegral <$> position), CDirection direction, CSpeed 100, CBrush 2)
  Apecs.modify Apecs.global $ \(CScore score) -> CScore $ Map.insert player 0 score
  pass
