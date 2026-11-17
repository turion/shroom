{- |
Module      : Control.Monad.Prompt.Promptable
Description : Request a value of a type from a language model, with a default strategy.

The class 'Promptable', the newtype 'DescriptionPrompt' to derive it via, and
'Promptable' instances for the base types. This module belongs to @shroom@; the
classes it builds on, 'Surveyable' and 'Describable', are in "Data.Shroom.Class"
from @shroom-class@.
-}
module Control.Monad.Prompt.Promptable (
  -- * Promptable
  Promptable (..),
  DescriptionPrompt (..),

  -- * Base types
  -- $baseTypes
) where

-- base
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (..))
import Numeric.Natural (Natural)

-- containers
import Data.Set (Set)

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- text
import Data.Text (Text)

-- time
import Data.Time (Day, UTCTime)

-- effectful
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State)

-- shroom

import Control.Monad.Prompt.Backend (ContextItem)
import Control.Monad.Prompt.Effect (Prompt)
import Control.Monad.Prompt.Effect qualified as Effect
import Data.Shroom.Class

{- | Attach a default prompting strategy to a type.

'prompt' is an 'Eff' program that requests a value of type @a@ from the model.
The default, 'Control.Monad.Prompt.Effect.promptSingle', sends the request to the
'Prompt' effect, which builds the prompt text from 'description' (the type's
description, properties and examples) and checks the model's answer against the
type's properties. Most types need no override, and an empty instance is enough:

@
instance Promptable User
@

Override 'prompt' to change what a request to this type does. The override below
adds one line of context to the default request, and keeps it out of every other
request:

@
instance Promptable Recipe where
  prompt = withContext "Always prefer metric units." promptSingle
@
-}
class (Surveyable a, ToSchema a, FromJSON a) => Promptable a where
  {- | Request a value of type @a@ from the model.

  An override can also add fallbacks or run sub-prompts in parallel. Its
  @State [ContextItem]@ constraint is the state that
  'Control.Monad.Prompt.Effect.context' adds to, so an override can scope context
  with 'Control.Monad.Prompt.Effect.withContext'.
  -}
  prompt :: forall es. (Prompt :> es, State [ContextItem] :> es) => Eff es a
  prompt = Effect.promptSingle

{- | Newtype for deriving the default 'Promptable' instance with @DerivingVia@, as an
alternative to the empty instance.

@
\$(deriveDescribable ''Recipe)

instance Surveyable Recipe

deriving via DescriptionPrompt Recipe instance Promptable Recipe
@
-}
newtype DescriptionPrompt a = DescriptionPrompt {getDescriptionPrompt :: a}
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToSchema)

instance (Describable a) => Describable (DescriptionPrompt a) where
  describeType _ = describeType (Proxy @a)

  -- @description@ runs at @DescriptionPrompt a@ here, so forward what @a@
  -- references. Not @reach @a@: that would list @a@ under itself.
  -- The three methods below are the contract of a transparent wrapper, see
  -- the 'describedIdentity' Haddock; 'Generically' is the other one.
  referencedTypes _ = referencedTypes (Proxy @a)
  hasOwnSection _ = False
  describedIdentity _ = describedIdentity (Proxy @a)

instance (Surveyable a) => Surveyable (DescriptionPrompt a) where
  type Property (DescriptionPrompt a) = Property a
  describeProperties _ = describeProperties (Proxy @a)
  propertyHolds (DescriptionPrompt x) = propertyHolds x

instance (Surveyable a, ToSchema a, FromJSON a) => Promptable (DescriptionPrompt a)

{- $baseTypes
#baseTypes#'Promptable' instances for the base types that "Data.Shroom.Class" gives
'Surveyable' instances: 'Text', 'Int', 'Integer', 'Int64', 'Word', 'Natural',
'Double', 'Float', 'Bool', 'Char', 'UTCTime', 'Day', and the containers @[a]@,
@'NonEmpty' a@, @'Set' a@ and @'Maybe' a@. Each uses the default 'prompt', so a
bare value is requested like any other type, from its 'description'.

__Schema keywords Anthropic rejects.__ @openapi3@ renders some of these types
with JSON Schema keywords that Anthropic's structured outputs answer with a
400, and that @normalizeSchemaForStructuredOutput@ does not strip: 'Char'
carries @minLength@ and @maxLength@, 'Natural' carries @exclusiveMinimum@, and
'Double' and 'Float' carry a numeric @format@ (not confirmed against the API).
A field of one of these types, or a bare 'prompt' of it, may therefore fail
against Claude. The container instances pass the element's schema through
unchanged, so they inherit the same limitation.
-}

-- | See "Control.Monad.Prompt.Promptable#baseTypes" for what Anthropic rejects.
instance Promptable Text

instance Promptable Int

instance Promptable Integer

instance Promptable Int64

instance Promptable Word

instance Promptable Natural

instance Promptable Double

instance Promptable Float

instance Promptable Bool

instance Promptable Char

instance Promptable UTCTime

instance Promptable Day

instance (Describable a, ToSchema a, FromJSON a) => Promptable [a]

instance (Describable a, ToSchema a, FromJSON a) => Promptable (NonEmpty a)

instance (Describable a, ToSchema a, FromJSON a, Ord a) => Promptable (Set a)

instance (Describable a, ToSchema a, FromJSON a) => Promptable (Maybe a)
