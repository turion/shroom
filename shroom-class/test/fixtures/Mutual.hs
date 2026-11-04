{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Bare instances for two types that are recursive through each other: rejected at compile time.
module Mutual where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Data.Shroom.Class

newtype A = A [B]
  deriving stock (Generic)
  deriving anyclass (ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

newtype B = B (Maybe A)
  deriving stock (Generic)
  deriving anyclass (ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance Describable A where
  describeType _ = "An A."

instance Describable B where
  describeType _ = "A B."

instance Surveyable A

instance Surveyable B
