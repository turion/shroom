{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A tool-testing report type parametrised by the list of tools under test.

'WebToolReport tools' holds a list of 'ToolResult' records, one per tool,
recording success (no error) or failure (error message).
-}
module WebToolReport (
  WebToolReport (..),
  ToolResult (..),
  expectedNames,
  toolResults,
  webToolReportChain,
) where

-- base
import Data.Kind (Type)
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

-- sop-core
import Data.SOP (All, K (..), NP, SListI, hcollapse, hcpure)

-- universe-base
import Data.Universe.Class (Universe)

-- shroom
import Control.Monad.Prompt (PromptT, context, prompt)
import Control.Monad.Prompt.Tool (Toolable, toolName)
import Data.Shroom.Class (Describable (..), Surveyable (..))
import Data.Shroom.Class.Promptable (DescriptionPrompt, Promptable)

-- * Types

-- | The result of testing one tool.
data ToolResult = ToolResult
  { resultToolName :: Text
  -- ^ The snake_case name of the tool that was tested.
  , resultStatus :: Text
  {- ^ Either @"ok"@ if the tool returned any content, or an error message
  string if the tool call failed with an exception or HTTP error.
  -}
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

{- | A report of tool tests.

A list of one 'ToolResult' per available tool.
-}
newtype WebToolReport (tools :: [Type]) = WebToolReport
  { results :: [ToolResult]
  -- ^ One entry per tool tested.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

{- | Extract results as a list of @(toolName, status)@ pairs.
Status is @"ok"@ for success or an error string for failure.
-}
toolResults :: WebToolReport tools -> [(Text, Text)]
toolResults r = (\ tr -> (resultToolName tr, resultStatus tr)) <$> results r

-- | The only enforced property: every expected tool must appear in the results.
data AllToolsTested = AllToolsTested
  deriving stock (Bounded, Enum, Eq, Ord, Show)
  deriving anyclass (Universe)

instance (SListI tools, All Toolable tools) => Describable (WebToolReport tools) where
  describeType _ =
    "A report of tool tests. "
      <> "Call each available tool with a reasonable test input. "
      <> "For each tool: set resultStatus to the string \"ok\" if the tool returned any content, "
      <> "or set resultStatus to the error message if the tool call failed with an exception or HTTP error. "
      <> "The tools to test are: "
      <> T.intercalate ", " (expectedNames @tools)
      <> ". Example of all tools succeeding: {\"results\": ["
      <> T.intercalate ", " (fmap (\k -> "{\"resultToolName\": \"" <> k <> "\", \"resultStatus\": \"ok\"}") (expectedNames @tools))
      <> "]}. "
      <> "Example with one failure: {\"results\": [{\"resultToolName\": \"some_tool\", \"resultStatus\": \"HTTP 404: not found\"}]}"

instance (SListI tools, All Toolable tools) => Surveyable (WebToolReport tools) where
  type Property (WebToolReport tools) = AllToolsTested

  describeProperties _ AllToolsTested =
    Just $
      "The results list must contain exactly one entry per tool: "
        <> T.intercalate ", " (expectedNames @tools)

  propertyHolds r AllToolsTested =
    let names = fmap resultToolName (results r)
     in all (`elem` names) (expectedNames @tools)

deriving via DescriptionPrompt (WebToolReport tools) instance (SListI tools, All Toolable tools, Typeable tools) => Promptable (WebToolReport tools)

-- | Extract the tool names for a type-level list of 'Toolable' types.
expectedNames ::
  forall tools.
  (SListI tools, All Toolable tools) =>
  [Text]
expectedNames =
  hcollapse (hcpure (Proxy @Toolable) nameK :: NP (K Text) tools)
  where
    nameK :: forall t. (Toolable t) => K Text t
    nameK = K (toolName (Proxy @t))

-- * Chain

{- | A prompt chain that asks the model to test every available tool and
return a 'WebToolReport'.  The type description already instructs the model
to record success\/failure per tool, so no extra context is needed.
-}
webToolReportChain ::
  forall tools m.
  (Monad m, SListI tools, All Toolable tools, Typeable tools) =>
  PromptT m (WebToolReport tools)
webToolReportChain = do
  context "You are a tool-testing assistant. Your job is to call each available tool exactly once with a reasonable test input, observe the result, then return a JSON report."
  context $ "Step 1: call each of these tools once: " <> T.intercalate ", " (expectedNames @tools) <> "."
  context $ "Step 2: return a JSON object with a \"results\" array. Each entry must have \"resultToolName\" and \"resultStatus\". Set resultStatus to \"ok\" if the tool returned any content. Set resultStatus to the error message if the tool result started with 'Error:'. Tools: " <> T.intercalate ", " (expectedNames @tools) <> "."
  prompt @(WebToolReport tools)
