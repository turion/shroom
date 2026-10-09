{- | The 'Promptable' typeclass: defines the default way to request a value of
a type from an LLM backend.

@
class (Surveyable a, ToSchema a, FromJSON a) => Promptable a where
  prompt :: forall es. (Prompt :> es) => Eff es a
  prompt = Effect.prompt
@

The default 'prompt' sends a 'Control.Monad.Prompt.Effect.RequestPrompt'
effect, which uses 'description' to build the prompt text from the type's
'Surveyable' metadata.

Override 'prompt' to add type-specific context, fallback logic, or any other
custom program:

@
instance Promptable MyType where
  prompt = withContext "Always prefer metric units." Effect.prompt
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
import Data.Proxy (Proxy (..))

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- effectful
import Effectful (Eff, (:>))

-- shroom

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
  or any other custom behaviour.
  -}
  prompt :: forall es. (Prompt :> es) => Eff es a
  prompt = Effect.prompt

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
