{-# LANGUAGE RecordWildCards #-}

module System.Input (system) where

import Apecs qualified
import Control.Monad.Except (throwError)
import Play (Command (End), readEvents)
import Relude
import Relude.Extra (member)
import SDL qualified
import World (System', tickState)
import World.Component

separateMotion :: SDL.KeyboardEventData -> Either SDL.Scancode SDL.Scancode
separateMotion e = if pressed then Left scancode else Right scancode
  where
    pressed = SDL.keyboardEventKeyMotion e == SDL.Pressed
    scancode = SDL.keysymScancode (SDL.keyboardEventKeysym e)

keyboardEventData :: SDL.EventPayload -> Maybe SDL.KeyboardEventData
keyboardEventData (SDL.KeyboardEvent e) = Just e
keyboardEventData _ = Nothing

applyBoth :: (t -> b) -> (t, t) -> (b, b)
applyBoth f (a, b) = (f a, f b)

isQuitEvent :: SDL.Event -> Bool
isQuitEvent (SDL.Event _t SDL.QuitEvent) = True
isQuitEvent _ = False

parseKeyboardInput :: (Set SDL.Scancode, Set SDL.Scancode) -> (SDL.Scancode -> Bool) -> KeyboardInput -> KeyboardInput
parseKeyboardInput (pressedKeys, releasedKeys) isKeyDown (KeyboardInput scancode _) =
  let keyPressed = scancode `member` pressedKeys
      keyReleased = scancode `member` releasedKeys
      keyDown = isKeyDown scancode
   in KeyboardInput scancode $ case (keyPressed, keyReleased, keyDown) of
        (True, _, _) -> KeyPressed
        (_, True, _) -> KeyReleased
        (_, _, True) -> KeyDown
        _ -> KeyUp

updateGameInput :: GameInput -> (KeyboardInput -> KeyboardInput) -> GameInput
updateGameInput GameInput {..} update =
  GameInput
    { playerInputs = (\PlayerInput {..} -> PlayerInput {turnLeft = update turnLeft, turnRight = update turnRight}) <$> playerInputs,
      reset = update reset,
      continue = update continue
    }

system :: System' ()
system = do
  events <- tickState readEvents
  when (any isQuitEvent events) . lift . throwError $ End

  isKeyDown <- SDL.getKeyboardState
  let payload = SDL.eventPayload <$> events
      keyboardEvents = mapMaybe keyboardEventData payload
      keySets = applyBoth (fromList @(Set SDL.Scancode)) . partitionEithers $ separateMotion <$> keyboardEvents
      update = parseKeyboardInput keySets isKeyDown
  (CScene schema) <- Apecs.get Apecs.global
  let updatedSchema = case schema of
        GameScene paused input -> GameScene paused $ updateGameInput input update
        MenuScene input -> MenuScene $ updateGameInput input update
        EndScene input -> EndScene $ updateGameInput input update
  Apecs.set Apecs.global (CScene updatedSchema)

  pass