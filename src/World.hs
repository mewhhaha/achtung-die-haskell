{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module World where

import Apecs
import Play (Command, Tick)
import Relude
import World.Component

makeWorld "World" [''CGameInput, ''CScene, ''CArena, ''CPosition, ''CDirection, ''CBrush, ''CDead, ''CPlayer, ''CScore, ''CResources, ''CSpeed]

type System' a = (Apecs.SystemT World (ReaderT (Tick World) (ExceptT Command IO))) a

instance MonadFail (Apecs.SystemT World (ReaderT (Tick World) (ExceptT Command IO))) where
  fail s = lift (fail s)

tickState :: (Tick World -> a) -> System' a
tickState f = lift $ asks f