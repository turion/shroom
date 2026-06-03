module Data.Shroom.Class.Promptable (module Data.Shroom.Class.Promptable) where

-- base
import Data.Proxy (Proxy (..))

-- text
import Data.Text (Text)

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi.Schema

-- shroom
import Data.Shroom.Class

-- * Promptable

{- | Determines how to render the full prompt description for a type.

The default 'promptDescription' calls 'description', which renders:

* 'describeType' — the one-sentence description
* properties from 'describeProperties'
* example values from 'examples'
* field types annotated from the OpenAPI schema

Override 'promptDescription' for fully custom prompt text.

Superclass: 'Surveyable' and 'ToSchema'.
-}
class (Surveyable a, ToSchema a) => Promptable a where
  {- | Render the full prompt fragment for a type, sent to the LLM alongside
    the JSON schema.
  -}
  promptDescription :: Proxy a -> Text

{- | Helper newtype for using the default description-based prompt rendering strategy.

Use with `DerivingVia`:

@
data MyType = MyType { ... }

-- TH call: Must go first
\$(deriveDescribable ''MyType)

instance Surveyable User where

-- Must come after TH call, therefore standalone deriving via
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

instance (Surveyable a, ToSchema a) => Promptable (DescriptionPrompt a) where
  promptDescription _ = description (Proxy @a)
