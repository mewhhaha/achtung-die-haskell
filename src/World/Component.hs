{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module World.Component where

import Apecs qualified
import Data.Map qualified as Map
import Linear (V2)
import Linear.V4 (V4)
import Numeric.Noise.Perlin (Perlin, perlin)
import Relude
import SDL qualified
import SDL.Font qualified as SDLFont

arenaWidth :: Int
arenaWidth = 480

arenaHeight :: Int
arenaHeight = 480

data CResources = CResources
  { font :: Maybe SDLFont.Font,
    texts :: Map (Text, V4 Word8) SDL.Texture
  }

instance Apecs.Component CResources where type Storage CResources = Apecs.Global CResources

instance Semigroup CResources where
  _ <> next = next

instance Monoid CResources where
  mempty =
    CResources
      { font = Nothing,
        texts = mempty
      }

data CArena = CArena
  { covered :: Map (V2 Int) Player,
    noise :: Perlin,
    texture :: Maybe SDL.Texture
  }

instance Apecs.Component CArena where type Storage CArena = Apecs.Global CArena

instance Semigroup CArena where
  _ <> next = next

instance Monoid CArena where
  mempty =
    CArena
      { covered = mempty,
        noise = perlin 1 5 0.05 0.5,
        texture = Nothing
      }

newtype CScore = CScore (Map Player Int)

instance Apecs.Component CScore where type Storage CScore = Apecs.Global CScore

instance Semigroup CScore where
  (CScore s1) <> (CScore s2) = CScore (s1 <> s2)

instance Monoid CScore where
  mempty = CScore mempty

data Player = Player1 | Player2 | Player3 | Player4 | Player5
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype CPlayer = CPlayer Player

instance Apecs.Component CPlayer where type Storage CPlayer = Apecs.Map CPlayer

newtype CBrush = CBrush Int

instance Apecs.Component CBrush where type Storage CBrush = Apecs.Map CBrush

newtype CColor = CColor (V4 Word8)

instance Apecs.Component CColor where type Storage CColor = Apecs.Map CColor

newtype CPosition = CPosition (V2 Double) deriving (Show)

instance Apecs.Component CPosition where type Storage CPosition = Apecs.Map CPosition

newtype CDirection = CDirection (V2 Double) deriving (Show)

instance Apecs.Component CDirection where type Storage CDirection = Apecs.Map CDirection

newtype CSpeed = CSpeed Double deriving (Show)

instance Apecs.Component CSpeed where type Storage CSpeed = Apecs.Map CSpeed

data CDead = CDead

instance Apecs.Component CDead where type Storage CDead = Apecs.Map CDead

-- Keyboard event components

data KeyboardState = KeyPressed | KeyDown | KeyReleased | KeyUp

data KeyboardInput = KeyboardInput SDL.Scancode KeyboardState

data PlayerInput = PlayerInput
  { turnLeft :: KeyboardInput,
    turnRight :: KeyboardInput
  }

data GameInput = GameInput
  { playerInputs :: Map Player PlayerInput,
    reset :: KeyboardInput,
    continue :: KeyboardInput
  }

defaultGameInput :: GameInput
defaultGameInput =
  GameInput
    { playerInputs =
        Map.fromList
          [ ( Player1,
              PlayerInput
                { turnLeft = KeyboardInput SDL.ScancodeLeft KeyUp,
                  turnRight = KeyboardInput SDL.ScancodeRight KeyUp
                }
            ),
            ( Player2,
              PlayerInput
                { turnLeft = KeyboardInput SDL.ScancodeA KeyUp,
                  turnRight = KeyboardInput SDL.ScancodeS KeyUp
                }
            ),
            ( Player3,
              PlayerInput
                { turnLeft = KeyboardInput SDL.ScancodeK KeyUp,
                  turnRight = KeyboardInput SDL.ScancodeL KeyUp
                }
            ),
            ( Player4,
              PlayerInput
                { turnLeft = KeyboardInput SDL.ScancodeV KeyUp,
                  turnRight = KeyboardInput SDL.ScancodeB KeyUp
                }
            ),
            ( Player5,
              PlayerInput
                { turnLeft = KeyboardInput SDL.ScancodeKP4 KeyUp,
                  turnRight = KeyboardInput SDL.ScancodeKP6 KeyUp
                }
            )
          ],
      reset = KeyboardInput SDL.ScancodeR KeyUp,
      continue = KeyboardInput SDL.ScancodeSpace KeyUp
    }

newtype CGameInput = CGameInput GameInput

instance Apecs.Component CGameInput where type Storage CGameInput = Apecs.Global CGameInput

instance Semigroup CGameInput where _ <> c2 = c2

instance Monoid CGameInput where
  mempty = CGameInput defaultGameInput

-- Scene components

data Paused = Paused | Unpaused
  deriving (Eq)

data Scene = MenuScene | GameScene Paused | EndScene

newtype CScene = CScene Scene

instance Apecs.Component CScene where type Storage CScene = Apecs.Global CScene

instance Semigroup CScene where _ <> c2 = c2

instance Monoid CScene where
  mempty = CScene MenuScene