{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | A bare instance for a type recursive through a list: rejected at compile time.
module RecTree where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Data.Shroom.Class

newtype RecTree = RecNode [RecTree]
  deriving stock (Generic)
  deriving anyclass (ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance Describable RecTree where
  describeType _ = "A tree."

instance Surveyable RecTree
