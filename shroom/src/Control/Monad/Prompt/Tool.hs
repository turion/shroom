{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Tool use support.

Define a tool as a Haskell type with a Haddock comment plus a handler, give
the program's type @'Tool' MySearch ':>' es@, offer
@'toolBinding' \@MySearch@ to 'Control.Monad.Prompt.Effect.promptTools', and
interpret with @'runTool' mySearchHandler@:

@
-- | A web search query.
data MySearch = MySearch { query :: Text }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

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
-}
module Control.Monad.Prompt.Tool (module Control.Monad.Prompt.Tool) where

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

{- | A type whose values the LLM can supply as tool inputs.

The LLM receives the tool name (derived from the type name in snake_case),
a description (from 'describeType', optionally extended by 'toolDescription'),
and the JSON schema of @t@.  When the model requests a tool call, the runner
parses the input as @t@, calls the 'ToolHandler', and feeds the 'Text'
result back before continuing.

Minimal complete definition: none — all methods have defaults.

Superclasses: 'Surveyable', 'ToSchema', 'FromJSON', 'Typeable' — deliberately
not 'Control.Monad.Prompt.Promptable.Promptable': a tool's input type is
described and schema'd the same way a 'Promptable' one is, but it is never
itself the target of a 'prompt' call, so requiring 'Promptable' would only
add an unused method and, worse, a dependency this module does not otherwise
need on "Control.Monad.Prompt.Effect".
-}
class (Surveyable t, ToSchema t, FromJSON t, Typeable t) => Toolable t where
  {- | Additional description appended to 'describeType' when sending tool
  metadata to the LLM.  Use this for operational details not obvious from
  the type (e.g. rate limits, return format).  'Nothing' means no extra text.
  -}
  toolDescription :: Proxy t -> Maybe Text
  toolDescription _ = Nothing

{- | Derive the tool name from the type name: convert @CamelCase@ to
@snake_case@, e.g. @WebSearch@ → @"web_search"@.
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

{- | A handler for tool @t@: receives the parsed input and returns either a
failure message (@Left@) or a success result (@Right@) fed back to the LLM.
Failures are reported to the model but do __not__ consume step budget.
-}
newtype ToolHandler t = ToolHandler
  { runToolHandler :: t -> IO (Either Text Text)
  }

-- * Tool metadata

{- | Extract 'ToolDef' values for all tools in an 'NP'.
Passed to backends so they can register the tools with the LLM API.
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

{- | A dynamic effect for calling tool @t@.  @'Tool' t ':>' es@ in a
program's constraints is what "a program's type names the tools it may
call" means: a program that tries to call a tool without that constraint
does not compile.  For example, given

@
badProgram :: (Prompt ':>' es) => Eff es (Either Text Text)
badProgram = callTool (WebFetch "https://example.com")
@

GHC (9.12.3, via @cabal repl@ against this module, 2026-09-29) reports:

@
    - Could not deduce \'Tool WebFetch :> es\'
        arising from a use of \'callTool\'
      from the context: Prompt :> es
        bound by the type signature for:
                   badProgram :: forall (es :: [Effect]).
                                 (Prompt :> es) =>
                                 Eff es (Either Text Text)
    - In the expression: callTool (WebFetch "https://example.com")
      In an equation for \'badProgram\':
          badProgram = callTool (WebFetch "https://example.com")
@

A handler is supplied as an interpreter — 'runTool' — rather than passed
as a runtime argument the way 'NP' 'ToolHandler' is: that is what "a
handler is supplied as an interpreter" means.  Because @es@ is an ordinary
type-level list, a sub-program needing only some of the caller's tools
(@(Tool A ':>' es) => Eff es b@) runs unchanged inside a caller whose @es@
also has @Tool B@ — a strict superset satisfies the subset's constraint
for free.
-}
data Tool t :: Effect where
  CallTool :: t -> Tool t m (Either Text Text)

type instance DispatchOf (Tool t) = Dynamic

-- | Call tool @t@. Requires @'Tool' t ':>' es@ — see the module Haddock.
callTool :: forall t es. (Tool t :> es) => t -> Eff es (Either Text Text)
callTool = send . CallTool

{- | Interpret @'Tool' t@ against a 'ToolHandler': this is the interpreter
that "supplies" the handler named in the module Haddock.
-}
runTool :: forall t es a. (IOE :> es) => ToolHandler t -> Eff (Tool t : es) a -> Eff es a
runTool (ToolHandler run) = interpret $ \_ (CallTool t) -> liftIO (run t)

{- | A tool offered to one particular prompt call: its 'ToolDef' metadata
paired with a way to invoke it inside the current effect stack.  Build one
with 'toolBinding'.
-}
data ToolBinding m = ToolBinding
  { toolBindingDef :: ToolDef
  , toolBindingCall :: Value -> m (Either Text Text)
  }

{- | Offer tool @t@ to a prompt call. Only compiles when @'Tool' t ':>' es@
is in scope — a program cannot offer a tool it was not itself given. Name
and description come from 'toolName' and 'describeType' \/ 'toolDescription',
exactly as for the 'NP' 'ToolHandler' path.
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

{- | Resolve a runtime @(tool_name, input_value)@ pair against a list of
'ToolBinding's — the dispatch half of the effect surface.  Each binding's
'toolBindingCall' has already closed over its own tool type, so this scans
a plain list keyed by name; that is the tool's arc bail-out firing for
/dispatch/ only: 'Tool' \/ 'toolBinding'\'s compile-time membership is
unaffected, and only the run-time lookup by name — unavoidable once the
model has emitted a bare tool name string — is a runtime registry.  See
@decisions.md@ for why full type-level dispatch was not attempted.
-}
dispatchBindings :: (Applicative m) => [ToolBinding m] -> Text -> Value -> m (Either Text Text)
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
