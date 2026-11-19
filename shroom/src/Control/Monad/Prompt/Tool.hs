{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
Module      : Control.Monad.Prompt.Tool
Description : Tools a language model can call, as effectful effects.

To complete an answer, a model may want to call a tool, for example a web
search with t'Control.Monad.Prompt.Tool.Web.DuckDuckGoSearch' or a lookup in your own
data. Tools run right here in your program, so a tool is an effect that the
model triggers: the program decides what a tool may do, and the model decides
when to call it. shroom gives the same grip on tools as on any other effect. A
program can only offer a tool whose @'Tool' t@ effect is in its effect row, and
the compiler checks that.

A tool is a Haskell type that the model fills in, together with a t'ToolHandler'
that runs it. The type is documented with a Haddock comment, which becomes the
description the model reads. A program lists the tools it may call in its
constraints, @'Tool' MySearch ':>' es@, offers them to the model with
'toolBinding' and 'Control.Monad.Prompt.Effect.promptTools', and is run with
'runTool' supplying the handler:

@
-- | A web search query.
data MySearch = MySearch { query :: Text }
  deriving (Generic, ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

\$(deriveDescribable ''MySearch)
instance Surveyable MySearch

instance Toolable MySearch where
  toolDescription _ = Just "Returns results from the search API."

mySearchHandler :: ToolHandler MySearch
mySearchHandler = ToolHandler $ \\(MySearch q) -> callSearchAPI q

myProgram :: (Tool MySearch ':>' es, Prompt ':>' es) => Eff es Result
myProgram = promptTools [toolBinding \@MySearch]

result <- runPromptResultEff backend defaultPromptConfig (runTool mySearchHandler myProgram)
@

"Control.Monad.Prompt.Tool.Web" provides ready-made tools for fetching pages and searching.
-}
module Control.Monad.Prompt.Tool (
  -- * Defining a tool
  Toolable (..),
  toolName,

  -- * Handling a tool
  ToolHandler (..),
  toolDefsRaw,

  -- * Tools as effects
  Tool (..),
  callTool,
  runTool,

  -- * Offering tools to the model
  ToolBinding (..),
  toolBinding,
  dispatchBindings,
) where

-- base
import Control.Monad.IO.Class (MonadIO (..))
import Data.Char (isUpper, toLower)
import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, tyConName, typeRep, typeRepTyCon)

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON, Result (..), Value (..), fromJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- sop-core
import Data.SOP (All, K (..), NP (..), SListI, hcmap, hcollapse)
import Data.SOP.NP ()

-- SListI instances

-- effectful
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)

-- shroom

import Control.Monad.Prompt.Schema (ToolDef (..), schemaWithDefs)
import Data.Shroom.Class (Surveyable, describeType)

-- * Tool typeclass

{- | A type whose values the model supplies as the input of a tool call.

The model is told the tool's name ('toolName'), a description made of
'describeType' followed by 'toolDescription', and the JSON schema of @t@.
When it calls the tool, the input is parsed as @t@ and passed to a t'ToolHandler',
whose result is sent back to the model.
-}
class (Surveyable t, ToSchema t, FromJSON t, Typeable t) => Toolable t where
  {- | Text appended to 'describeType' in the tool's description, for details
  the type does not show, such as rate limits or the format of the result.
  'Nothing' adds nothing.
  -}
  toolDescription :: Proxy t -> Maybe Text
  toolDescription _ = Nothing

{- | The name the model calls tool @t@ by: the type's name in snake case, so
@WebSearch@ becomes @"web_search"@.
-}
toolName :: forall t. (Typeable t) => Proxy t -> Text
toolName p =
  let name = tyConName (typeRepTyCon (typeRep p))
   in T.pack $ fmap toLower $ intercalate "_" $ splitCamel name
  where
    splitCamel [] = []
    splitCamel (c : cs) =
      let (word, rest) = break isUpper cs
       in (c : word) : splitCamel rest

-- * Tool handler

{- | The code that runs tool @t@: it receives the parsed input and returns
@Left@ with a failure message or @Right@ with a result, either of which is
sent back to the model. A failure does __not__ count against the limit on tool rounds.
-}
newtype ToolHandler t = ToolHandler
  { runToolHandler :: t -> IO (Either Text Text)
  -- ^ Run the tool on one input.
  }

-- * Tool metadata

{- | The t'ToolDef' of every tool in a list of handlers, for a backend to
register with the model's API.
-}
toolDefsRaw ::
  forall tools.
  (SListI tools, All Toolable tools) =>
  NP ToolHandler tools ->
  [ToolDef]
toolDefsRaw = hcollapse . hcmap (Proxy @Toolable) extract
  where
    extract :: forall t. (Toolable t) => ToolHandler t -> K ToolDef t
    extract _ =
      K
        ToolDef
          { toolDefName = toolName p
          , toolDefDescription = describeType p <> maybe "" (" " <>) (toolDescription p)
          , toolDefSchema = schemaWithDefs p
          }
      where
        p = Proxy @t

-- * Tools as effects

{- | The effect of calling tool @t@.

A program that calls a tool has @'Tool' t ':>' es@ in its constraints, and
calling it without that constraint does not type-check. A sub-program that
needs only some tools runs unchanged inside a program that has more.
'runTool' interprets the effect with a t'ToolHandler'.
-}
data Tool t :: Effect where
  -- | Call the tool with the given input.
  CallTool :: t -> Tool t m (Either Text Text)

type instance DispatchOf (Tool t) = Dynamic

-- | Call tool @t@ and return its result, or the failure message of its handler.
callTool :: forall t es. (Tool t :> es) => t -> Eff es (Either Text Text)
callTool = send . CallTool

-- | Interpret @'Tool' t@ with a t'ToolHandler', running the handler in 'IO'.
runTool :: forall t es a. (IOE :> es) => ToolHandler t -> Eff (Tool t : es) a -> Eff es a
runTool (ToolHandler run) = interpret $ \_ (CallTool t) -> liftIO (run t)

-- * Offering tools to the model

{- | A tool offered to one prompt call: its t'ToolDef' paired with the action
that calls it. Build one with 'toolBinding'.
-}
data ToolBinding m = ToolBinding
  { toolBindingDef :: ToolDef
  -- ^ The name, description and schema the model is sent.
  , toolBindingCall :: Value -> m (Either Text Text)
  -- ^ Parse the model's input and call the tool; a parse failure is a @Left@.
  }

{- | Offer tool @t@ to a prompt call. It needs @'Tool' t ':>' es@, so a program
can only offer a tool it has been given. The name and description are those
of 'toolDefsRaw'.
-}
toolBinding :: forall t es. (Toolable t, Tool t :> es) => ToolBinding (Eff es)
toolBinding =
  ToolBinding
    { toolBindingDef =
        ToolDef
          { toolDefName = toolName p
          , toolDefDescription = describeType p <> maybe "" (" " <>) (toolDescription p)
          , toolDefSchema = schemaWithDefs p
          }
    , toolBindingCall = \v -> case fromJSON v of
        Error e -> pure (Left ("Tool input parse error for " <> toolName p <> ": " <> T.pack e))
        Success (t :: t) -> callTool t
    }
  where
    p = Proxy @t

{- | Call the tool that the model named, on the input it sent.

The lookup by name happens at run time, since the model sends only a name.
-}
dispatchBindings ::
  (Applicative m) =>
  -- | The tools on offer.
  [ToolBinding m] ->
  {- | The name of the tool, as the model sent it. An unknown name gives a @Left@ that
  lists the available tools.
  -}
  Text ->
  -- | The input of the tool, as the model sent it.
  Value ->
  m (Either Text Text)
dispatchBindings bindings name v =
  case [call | ToolBinding {toolBindingDef, toolBindingCall = call} <- bindings, toolDefName toolBindingDef == name] of
    (call : _) -> call v
    [] ->
      pure $
        Left
          ( "Unknown tool: "
              <> name
              <> ". Available tools: "
              <> T.intercalate ", " (fmap (toolDefName . toolBindingDef) bindings)
          )
