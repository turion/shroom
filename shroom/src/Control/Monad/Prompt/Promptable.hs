{- | The 'Promptable' typeclass: defines the default way to request a value of
a type from an LLM backend.

@
class (Surveyable a, ToSchema a, FromJSON a) => Promptable a where
  prompt :: forall es. (Prompt :> es, State [ContextItem] :> es) => Eff es a
  prompt = Effect.promptSingle
@

The default 'prompt' is 'Control.Monad.Prompt.Effect.promptSingle': it sends a
'Control.Monad.Prompt.Effect.RequestPrompt' effect, which uses 'description'
to build the prompt text from the type's 'Surveyable' metadata.

Override 'prompt' to add type-specific context, fallback logic, or any other
custom program. The method has @State [ContextItem]@ in scope, so an override
can scope context with 'Control.Monad.Prompt.Effect.withContext':

@
instance Promptable MyType where
  prompt = withContext "Always prefer metric units." Effect.promptSingle
@

Typical usage (no override needed):

@
\$(deriveDescribable ''MyType)

instance Surveyable MyType where ...

instance Promptable MyType
@

== Pure\/effectful boundary

This module is the effectful side of the line drawn against "Data.Shroom.Class",
and the package boundary coincides with it: "Data.Shroom.Class" lives in
@shroom-class@, this module in @shroom@.
'Promptable'\'s method is an 'Eff' program requiring the 'Prompt' effect, so
anything that names 'Promptable' or 'DescriptionPrompt' depends on the
program layer and must live under "Control.Monad.Prompt", never under
@Data.Shroom.@. Nothing under @Data.Shroom.@ may import this module.
-}
module Control.Monad.Prompt.Promptable (module Control.Monad.Prompt.Promptable) where

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

-- * Promptable

{- | Attach a default prompting strategy to a type.

The single method 'prompt' is an 'Eff' program that requests a value of
type @a@ from the LLM.  The default implementation sends
'Control.Monad.Prompt.Effect.RequestPrompt', which builds the prompt from
'description' (the type description, properties, and examples from the
'Surveyable' instance).

Superclasses: 'Surveyable', 'ToSchema', 'FromJSON'.
-}
class (Surveyable a, ToSchema a, FromJSON a) => Promptable a where
  {- | An 'Eff' program that requests a value of type @a@ from the LLM.

  Override to add type-specific context, fallback logic, parallel sub-prompts,
  or any other custom behaviour. The @State [ContextItem]@ constraint is what
  lets an override scope context with 'Control.Monad.Prompt.Effect.withContext'
  or 'Control.Monad.Prompt.Effect.withContextItem'; it is the same state
  'Control.Monad.Prompt.Effect.context' adds to, so a caller that already
  chains prompts has it.
  -}
  prompt :: forall es. (Prompt :> es, State [ContextItem] :> es) => Eff es a
  prompt = Effect.promptSingle

{- | Helper newtype for using the default description-based prompting strategy
via @DerivingVia@.

@
data MyType = MyType { ... }

\$(deriveDescribable ''MyType)

instance Surveyable MyType where ...

-- Must come after TH call, hence standalone deriving:
deriving via DescriptionPrompt MyType instance Promptable MyType
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

-- | Uses the default 'prompt' = 'Control.Monad.Prompt.Effect.promptSingle'.
instance (Surveyable a, ToSchema a, FromJSON a) => Promptable (DescriptionPrompt a)

-- * Base types

{- | Flat 'Promptable' instances for the base types that "Data.Shroom.Class"
gives 'Surveyable' instances: 'Text', 'Int', 'Integer', 'Int64', 'Word', 'Natural', 'Double',
'Float', 'Bool', 'Char', 'UTCTime', 'Day', and the containers @[a]@,
@'NonEmpty' a@, @'Set' a@ and @'Maybe' a@. Each uses the default 'prompt', so a bare value
is requested like any other type, from its 'description'.

They live in this module, next to the class, so they are not orphans.

__Schema keywords Anthropic rejects.__ @openapi3@ renders some of these types
with JSON Schema keywords that Anthropic's structured outputs answer with a
400, and that @normalizeSchemaForStructuredOutput@
does not strip: 'Char' carries @minLength@ and @maxLength@, 'Natural' carries
@exclusiveMinimum@, and 'Double' and 'Float' carry a numeric @format@ (not
confirmed against the API). A field of one of these types, or a bare
'prompt' of it, may therefore fail against Claude until the normaliser strips
those keywords. The container instances pass the element's schema through
unchanged, so they inherit the same limitation.
-}
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
