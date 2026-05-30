{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A tool-testing report type parametrised by the list of tools under test.

'WebToolReport tools' holds a 'Map' from each tool name to 'Nothing' (success)
or 'Just' an error description (failure).  The 'Describe' instance uses the
phantom @tools@ list to enumerate expected keys and emit invariants.
-}
module WebToolReport (
  WebToolReport (..),
  WebToolReportProperty (..),
  expectedNames,
  webToolReportChain,
) where

-- base
import Data.Kind (Type)
import Data.Maybe (isNothing)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- containers
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

-- sop-core
import Data.SOP (All, K (..), NP, SListI, hcollapse, hcpure)

-- universe-base
import Data.Universe.Class (Universe)

-- shroom
import Control.Monad.Prompt (PromptT, context, prompt)
import Control.Monad.Prompt.Tool (IsTool, toolName)
import Data.Describe (Describe (..))

-- * Type

{- | A report of tool tests.

For each available tool, attempt to call it within its documented scope.
Record @null@ (JSON) on success, or an error string on failure.
-}
newtype WebToolReport (tools :: [Type]) = WebToolReport
  { toolResults :: Map Text (Maybe Text)
  -- ^ Tool name → @Nothing@ (success) or @Just errorMsg@ (failure).
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Properties a 'WebToolReport' must satisfy.
data WebToolReportProperty
  = -- | The map keys must be exactly the names of the offered tools.
    AllToolsTested
  | -- | Every value must be @Nothing@ (all tools succeeded).
    AllToolsSucceeded
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

instance (SListI tools, All IsTool tools) => Describe (WebToolReport tools) where
  describeType _ =
    "A report of tool tests. "
      <> "For each available tool, attempt to call it within its documented scope. "
      <> "Record null on success, or an error string on failure. "
      <> "The keys must be exactly: "
      <> T.intercalate ", " (expectedNames @tools)

  type Property (WebToolReport tools) = WebToolReportProperty

  describeProperties _ AllToolsTested =
    Just $
      "The toolResults map must contain exactly these keys: "
        <> T.intercalate ", " (expectedNames @tools)
  describeProperties _ AllToolsSucceeded =
    Just "Every value in toolResults must be null (Nothing), meaning all tools succeeded."

  propertyHolds r AllToolsTested =
    Map.keysSet (toolResults r) == Set.fromList (expectedNames @tools)
  propertyHolds r AllToolsSucceeded =
    all isNothing (Map.elems (toolResults r))

-- | Extract the tool names for a type-level list of 'IsTool' types.
expectedNames ::
  forall tools.
  (SListI tools, All IsTool tools) =>
  [Text]
expectedNames =
  hcollapse (hcpure (Proxy @IsTool) nameK :: NP (K Text) tools)
  where
    nameK :: forall t. (IsTool t) => K Text t
    nameK = K (toolName (Proxy @t))

-- * Chain

{- | A prompt chain that asks the model to test every available tool and
return a 'WebToolReport'.  The type description already instructs the model
to record success\/failure per tool, so no extra context is needed.
-}
webToolReportChain ::
  forall tools m.
  (Monad m, SListI tools, All IsTool tools, Typeable tools) =>
  PromptT m (WebToolReport tools)
webToolReportChain = do
  context "You are a tool-testing assistant."
  prompt @(WebToolReport tools)
