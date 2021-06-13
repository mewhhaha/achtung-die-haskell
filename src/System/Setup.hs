module System.Setup (system) where

import Apecs (System, global, set)
import Linear
import Numeric.Noise.Perlin (perlin)
import Relude
import SDL qualified
import SDL.Font qualified as SDLFont
import System.FilePath ((</>))
import World (World)
import World.Component
  ( CArena (CArena),
    CResources (CResources),
    arenaHeight,
    arenaWidth,
  )

system :: SDL.Window -> SDL.Renderer -> System World ()
system _ renderer = do
  texture <- SDL.createTexture renderer SDL.RGB24 SDL.TextureAccessTarget (fromIntegral <$> V2 arenaWidth arenaHeight)
  Apecs.set Apecs.global (CArena mempty (perlin 1 5 0.05 0.5) (Just texture))

  font <- SDLFont.load ("assets" </> "font" </> "Schoolgirls.otf") 64
  Apecs.set Apecs.global (CResources (Just font) mempty)
