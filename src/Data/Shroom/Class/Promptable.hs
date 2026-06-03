{- | The 'Promptable' typeclass: defines the default way to request a value of
a type from an LLM backend.

@
class (Surveyable a, ToSchema a, FromJSON a) => Promptable a where
  prompt :: forall m. PromptT m a
  prompt = PromptSingle
@

The default 'prompt' sends a 'PromptSingle' request, which uses 'description'
to build the prompt text from the type's 'Surveyable' metadata.

Override 'prompt' to add type-specific context, fallback logic, or any other
custom 'PromptT' program:

@
instance Promptable MyType where
  prompt = WithContext (UserMessage "Always prefer metric units.") PromptSingle
@

Typical usage (no override needed):

@
\$(deriveDescribable ''MyType)

instance Surveyable MyType where ...

deriving via DescriptionPrompt MyType instance Promptable MyType
@
-}
module Data.Shroom.Class.Promptable (module Data.Shroom.Class.Promptable) where

-- base
import Data.Proxy (Proxy (..))

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- shroom

import Control.Monad.Prompt.Core (PromptT (..))
import Data.Shroom.Class

-- * Promptable

{- | Attach a default prompting strategy to a type.

The single method 'prompt' is a 'PromptT' program that requests a value of
type @a@ from the LLM.  The default implementation uses 'PromptSingle', which
builds the prompt from 'description' (the type description, properties, and
examples from the 'Surveyable' instance).

Superclasses: 'Surveyable', 'ToSchema', 'FromJSON'.
-}
class (Surveyable a, ToSchema a, FromJSON a) => Promptable a where
  {- | A 'PromptT' program that requests a value of type @a@ from the LLM.

  Override to add type-specific context, fallback logic, parallel sub-prompts,
  or any other custom behaviour.
  -}
  prompt :: forall m. PromptT m a
  prompt = PromptSingle

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

instance (Surveyable a) => Surveyable (DescriptionPrompt a) where
  type Property (DescriptionPrompt a) = Property a
  describeProperties _ = describeProperties (Proxy @a)
  propertyHolds (DescriptionPrompt x) = propertyHolds x

-- | Uses the default 'prompt' = 'PromptSingle'.
instance (Surveyable a, ToSchema a, FromJSON a) => Promptable (DescriptionPrompt a)
