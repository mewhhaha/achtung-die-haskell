{-# LANGUAGE RecordWildCards #-}

module System.Overlay (system) where

import Apecs qualified
import Data.Map qualified as Map
import Data.Set qualified as Set
import Foreign.C
import Linear (V2 (V2))
import Linear.V4
import Play (Tick (readWindow), readRenderer)
import Player.Color qualified as PlayerColor
import Relude
import SDL qualified
import SDL.Font qualified as SDLFont
import World (System', tickState)
import World.Component (CPlayer (..), CResources (..), CScene (..), CScore (..), GameInput (..), KeyboardInput (..), Player (..), PlayerInput (..), Scene (EndScene, GameScene, MenuScene), arenaWidth, turnLeft, turnRight)

createTextTexture :: Text -> V4 Word8 -> System' SDL.Texture
createTextTexture fontText fontColor = do
  renderer <- tickState readRenderer
  (CResources (Just font) texts) <- Apecs.get Apecs.global
  case Map.lookup (fontText, fontColor) texts of
    Just t -> return t
    Nothing -> do
      surface <- SDLFont.blended font fontColor fontText
      texture <- SDL.createTextureFromSurface renderer surface
      SDL.freeSurface surface
      Apecs.modify Apecs.global (\resources -> resources {texts = Map.insert (fontText, fontColor) texture texts})
      return texture

playerTextPosition :: Player -> CInt
playerTextPosition player =
  let gapSize = 70
   in fromIntegral (fromEnum player * gapSize)

drawText :: Text -> V4 Word8 -> V2 CInt -> System' ()
drawText fontText fontColor fontPosition = do
  renderer <- tickState readRenderer
  texture <- createTextTexture fontText fontColor
  info <- SDL.queryTexture texture
  let size = V2 (SDL.textureWidth info) (SDL.textureHeight info)
  SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (SDL.P $ fontPosition + V2 (- (SDL.textureWidth info `div` 2)) 0) size)

drawTextBox :: Text -> V4 Word8 -> V2 CInt -> System' (V2 CInt)
drawTextBox fontText fontColor fontPosition = do
  renderer <- tickState readRenderer
  texture <- createTextTexture fontText fontColor
  info <- SDL.queryTexture texture
  let size = V2 (SDL.textureWidth info) (SDL.textureHeight info)
      position = fontPosition + V2 (- (SDL.textureWidth info `div` 2)) 0
  SDL.rendererDrawColor renderer SDL.$= fontColor
  SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (SDL.P position) size)
  SDL.drawRect renderer (Just $ SDL.Rectangle (SDL.P position) size)
  pure size

gameSceneOverlay :: System' ()
gameSceneOverlay = do
  window <- tickState readWindow
  renderer <- tickState readRenderer
  (V2 windowWidth windowHeight) <- SDL.get (SDL.windowSize window)

  let sidebarSize = V2 (windowWidth - fromIntegral arenaWidth) windowHeight
  SDL.rendererRenderTarget renderer SDL.$= Nothing
  SDL.rendererDrawColor renderer SDL.$= minBound
  SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P (V2 (fromIntegral arenaWidth) 0)) sidebarSize)
  SDL.rendererDrawColor renderer SDL.$= maxBound
  SDL.drawRect renderer (Just $ SDL.Rectangle (SDL.P (V2 (fromIntegral arenaWidth) 0)) sidebarSize)

  Apecs.cmapM_ $ \(CPlayer player, CScore score) ->
    case Map.lookup player score of
      Just s -> do
        drawText (show s) (PlayerColor.fromPlayer player) (V2 (fromIntegral arenaWidth + 60) (playerTextPosition player + 40))
      Nothing -> pass

drawControls :: V4 Word8 -> Text -> Text -> V2 CInt -> System' ()
drawControls color leftKey rightKey position = do
  (V2 w _) <- drawTextBox leftKey color position
  _ <- drawTextBox rightKey color (position + V2 (w + 20) 0)
  pass

showControl :: KeyboardInput -> Maybe Text
showControl (KeyboardInput code _) = case code of
  SDL.ScancodeLeft -> Just "<"
  SDL.ScancodeRight -> Just ">"
  SDL.ScancodeA -> Just "A"
  SDL.ScancodeS -> Just "S"
  SDL.ScancodeK -> Just "K"
  SDL.ScancodeL -> Just "L"
  SDL.ScancodeV -> Just "V"
  SDL.ScancodeB -> Just "B"
  SDL.ScancodeKP4 -> Just "4"
  SDL.ScancodeKP6 -> Just "6"
  _ -> Nothing

menuSceneOverlay :: Map Player PlayerInput -> System' ()
menuSceneOverlay playerInputs = do
  window <- tickState readWindow
  (V2 windowWidth windowHeight) <- SDL.get (SDL.windowSize window)

  lockedInPlayers <- Apecs.cfold (\m (CPlayer player) -> Set.insert player m) mempty

  forM_ (zip (Map.assocs playerInputs) [0 ..]) $ \((player, PlayerInput {turnLeft, turnRight}), index) -> do
    let color = if Set.member player lockedInPlayers then PlayerColor.fromPlayer player else pure 100
        paddingWidth = windowWidth `div` 5
        paddingHeight = windowWidth `div` 4
        width = (windowWidth - paddingWidth) `div` 3
        height = (windowHeight - paddingHeight) `div` 2
        calcWidth = paddingWidth + (index `mod` 3) * width
        calcHeight = paddingHeight + (index `div` 3) * height

    case (showControl turnLeft, showControl turnRight) of
      (Just leftKey, Just rightKey) -> do
        drawControls color leftKey rightKey (V2 calcWidth calcHeight)
      _ -> pass
  drawText "Apecs die kurve" maxBound (V2 (windowWidth `div` 2) (windowHeight `div` 10))

endSceneOverlay :: System' ()
endSceneOverlay = pass

system :: System' ()
system = do
  (CScene scene) <- Apecs.get Apecs.global
  renderer <- tickState readRenderer
  SDL.rendererRenderTarget renderer SDL.$= Nothing
  case scene of
    GameScene {} -> gameSceneOverlay
    MenuScene GameInput {..} -> menuSceneOverlay playerInputs
    EndScene {} -> endSceneOverlay
