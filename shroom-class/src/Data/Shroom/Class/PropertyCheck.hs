{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | One property as a value: what the model is told, and the check.

This module imports nothing from "Data.Shroom.Class", which re-exports
everything here.
-}
module Data.Shroom.Class.PropertyCheck (
  PropertyCheck (..),
  propertyLines,
) where

-- base
import Data.Maybe (mapMaybe)

-- text
import Data.Text (Text)

{- | A property that generated values need to satisfy: the text the model is
told, and the check.

Both fields are read with @OverloadedRecordDot@, as @c.description@ and
@c.holds@: the module has @NoFieldSelectors@, so that the field @description@
does not clash with the function 'Data.Shroom.Class.description'.

Build a value with record syntax, or positionally:

@
PropertyCheck {description = Just "The number should be even", holds = even}
PropertyCheck (Just "The number should be even") even
@

Record construction needs @GHC2024@ or @DisambiguateRecordFields@ in the
building module whenever the 'Data.Shroom.Class.description' function is also
in scope, as it is after @import Data.Shroom.Class@; under @Haskell2010@ or
@GHC2021@ it is an \"Ambiguous occurrence\". There, build the value
positionally. Reading a field needs @OverloadedRecordDot@.
-}
data PropertyCheck a = PropertyCheck
  { description :: Maybe Text
  {- ^ What the model is told about this property, to help it create values
  that satisfy it, e.g. @Just "The number should be even"@. 'Nothing':
  left out of the prompt's property list.
  -}
  , holds :: a -> Bool
  -- ^ Whether the property holds of a value, e.g. 'even'.
  }

{- | The @"- <description>"@ lines of a list of properties, in order; a
property without a description is left out.
-}
propertyLines :: [PropertyCheck a] -> [Text]
propertyLines = fmap ("- " <>) . mapMaybe (\c -> c.description)
