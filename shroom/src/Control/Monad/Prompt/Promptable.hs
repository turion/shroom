{- |
Module      : Control.Monad.Prompt.Promptable
Description : Request a value of a type from a language model, with a default strategy.

Ask a language model for a value of one of your own types. You write the type
with Haddock comments on it and on its fields, derive the classes that describe
it in a few lines of boilerplate, and the type is ready to be prompted for. The
model is shown your Haddocks as the description of what to produce, and its
answer is parsed into your type and checked against the type's properties.

@
-- | A recipe for a dish.
data Recipe = Recipe
  { title :: Text
  -- ^ The name of the dish.
  , servings :: Int
  -- ^ How many people the recipe feeds.
  , steps :: [Text]
  -- ^ The preparation steps, in order.
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

\$(deriveDescribable ''Recipe)

instance Surveyable Recipe

instance Promptable Recipe
@

Here @SOP@ is "Generics.SOP". Running 'prompt' at the type @Recipe@ then requests
a recipe from the model. The classes this module builds on, 'Surveyable' and
'Describable', are in "Data.Shroom.Class" from @shroom-class@.
-}
module Control.Monad.Prompt.Promptable (
  -- * Promptable
  Promptable (..),
  DescriptionPrompt (..),
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

{- | A class for types that you want to request from a language model.

An instance says that a value of the type can be asked for with 'prompt'. The
superclasses carry what the model needs. 'Surveyable' supplies the description,
properties and examples that the prompt is built from. 'ToSchema' supplies the
schema that the model's answer must follow, and 'FromJSON' parses the answer.

Most types need nothing more, and an empty instance is enough. The request
sends the type's 'description' to the 'Prompt' effect with
'Control.Monad.Prompt.Effect.promptSingle', and checks the answer against the
type's properties:

@
instance Promptable Recipe
@

Override 'prompt' to change what a request for your type does. The override
below adds one line of context to the request, and keeps it out of every other
request:

@
instance Promptable Recipe where
  prompt = withContext "Always prefer metric units." promptSingle
@

To prompt a record part by part instead, see "Control.Monad.Prompt.Generic".
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

-- | Default prompt.
instance (Surveyable a, ToSchema a, FromJSON a) => Promptable (DescriptionPrompt a)

-- | Default prompt.
instance Promptable Text

-- | Default prompt.
instance Promptable Int

-- | Default prompt.
instance Promptable Integer

-- | Default prompt.
instance Promptable Int64

-- | Default prompt.
instance Promptable Word

-- | Default prompt.
instance Promptable Natural

-- | Default prompt.
instance Promptable Double

-- | Default prompt.
instance Promptable Float

-- | Default prompt.
instance Promptable Bool

-- | Default prompt.
instance Promptable Char

-- | Default prompt.
instance Promptable UTCTime

-- | Default prompt.
instance Promptable Day

-- | Default prompt, for a list of values described by @a@.
instance (Describable a, ToSchema a, FromJSON a) => Promptable [a]

-- | Default prompt, for a non-empty list of values described by @a@.
instance (Describable a, ToSchema a, FromJSON a) => Promptable (NonEmpty a)

-- | Default prompt, for a set of values described by @a@.
instance (Describable a, ToSchema a, FromJSON a, Ord a) => Promptable (Set a)

-- | Default prompt, for an optional value described by @a@.
instance (Describable a, ToSchema a, FromJSON a) => Promptable (Maybe a)
