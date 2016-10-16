{-# LANGUAGE GADTs #-}
module DBOp where

import Prelude hiding (Read)

{- DB operations DSL -}

data Create
data Read
data Update
data Delete

data DBOp o r a where
  Find :: DBOp Read r a
  Create :: DBOp Create r a
  Chain  :: (a -> DBOp o r b) -> DBOp o r a -> DBOp o r b
  Map    :: (a -> b) -> DBOp o r a -> DBOp o r b
