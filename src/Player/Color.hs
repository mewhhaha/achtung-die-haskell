module Player.Color where

import Linear (V4 (..))
import Relude (Maybe (..), Word8)
import World.Component (Player (..))

-- >>> toPlayer (fromPlayer Player1)
-- >>> toPlayer (fromPlayer Player2)
-- Just Player1
-- Just Player2

fromPlayer :: Player -> V4 Word8
fromPlayer Player1 = V4 255 0 0 0
fromPlayer Player2 = V4 0 255 0 0
fromPlayer Player3 = V4 0 0 255 0
fromPlayer Player4 = V4 255 255 0 0
fromPlayer Player5 = V4 0 255 255 0

toPlayer :: V4 Word8 -> Maybe Player
toPlayer (V4 255 0 0 0) = Just Player1
toPlayer (V4 0 255 0 0) = Just Player2
toPlayer (V4 0 0 255 0) = Just Player3
toPlayer (V4 255 255 0 0) = Just Player4
toPlayer (V4 0 255 255 0) = Just Player5
toPlayer _ = Nothing
