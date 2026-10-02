{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{- | Non-object top-level response types, used only in "Integration.Ollama"
to prove that a bare scalar or array root schema still round-trips through
a real local model now that the old @Control.Monad.Prompt.Ollama@ module —
deleted, along with its @{\"result\": ...}@ wrapper — is gone: @baikai@\'s own
'Baikai.ResponseFormat.jsonSchemaFormat' passes the schema straight through
as a raw JSON 'Data.Aeson.Value', so there is nothing left to unwrap.

Each is a thin newtype over the type it stands in for ('Int', 'Text',
@['Int']@) rather than an instance directly on that type, so the
'Describable'\/'Surveyable'\/'Promptable' instances live in the module that
defines the newtype and are not orphans. This changes nothing about the
wire shape: a single-constructor, single-field newtype with no record
selector derives 'Data.Aeson.ToJSON'\/'Data.OpenApi.ToSchema' identically to
its underlying type via @GHC.Generics@ — confirmed directly in
@cabal repl@, and already relied on by "Types"\'s own 'Types.Counter', which
this module otherwise mirrors.
-}
module ScalarTypes (ScalarInt (..), ScalarText (..), ScalarIntList (..)) where

-- base
import GHC.Generics (Generic)

-- text
import Data.Text (Text)

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- shroom
import Control.Monad.Prompt.Promptable (Promptable)
import Data.Shroom.Class (Describable (..), Surveyable)

-- | A bare top-level integer response.
newtype ScalarInt = ScalarInt Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Describable ScalarInt where
  describeType _ = "An integer."

instance Surveyable ScalarInt

instance Promptable ScalarInt

-- | A bare top-level string response.
newtype ScalarText = ScalarText Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Describable ScalarText where
  describeType _ = "A short string."

instance Surveyable ScalarText

instance Promptable ScalarText

-- | A bare top-level array-of-integers response.
newtype ScalarIntList = ScalarIntList [Int]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Describable ScalarIntList where
  describeType _ = "A JSON array of integers."

instance Surveyable ScalarIntList

instance Promptable ScalarIntList
