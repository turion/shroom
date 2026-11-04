{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- | The control of the rejection tests: a module that compiles in the same
environment, with a bare instance and a recursive type that opts out.
-}
module Accepted where

import Data.Aeson (ToJSON)
import Data.Void (Void)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Data.Shroom.Class

data Leafy = Leafy Int Bool
  deriving stock (Generic)
  deriving anyclass (ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

newtype Tree = Node [Tree]
  deriving stock (Generic)
  deriving anyclass (ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance Describable Leafy where
  describeType _ = "A leafy."

instance Describable Tree where
  describeType _ = "A tree."

instance Surveyable Leafy

instance Surveyable Tree where
  type Property Tree = Void
