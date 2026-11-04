{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- | A bare instance for a type recursive through a polymorphic type that is
instantiated at the recursive one: rejected at compile time.
-}
module Grouping where

import Data.Aeson (ToJSON)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Data.Shroom.Class

-- | A condition: a leaf, with no properties.
newtype Cond = Cond Int
  deriving stock (Generic)
  deriving anyclass (ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

data Branch c g = Branch [c] (Maybe g)
  deriving stock (Generic)
  deriving anyclass (ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

newtype Grouping = Grouping (NonEmpty (Branch Cond Grouping))
  deriving stock (Generic)
  deriving anyclass (ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance Describable Cond where
  describeType _ = "A condition."

instance (Describable c, Describable g) => Describable (Branch c g) where
  describeType _ = "A branch."

instance Describable Grouping where
  describeType _ = "A grouping."

instance Surveyable Cond

-- This one is fine: the recursion is not visible until it is instantiated.
instance (Surveyable c, Surveyable g) => Surveyable (Branch c g)

instance Surveyable Grouping
