{-# LANGUAGE DeriveAnyClass #-}

{- | A contract test for the Anthropic path, run against Bifrost in front of
Ollama: 'anthropicCompatBackend' pointed at an Anthropic Messages proxy that
this suite does not start and that, being Bifrost, is nothing this repository
wrote.

= What it is, and what it is not

It is a contract test against /Bifrost-in-front-of-Ollama/. It is __not__ a
test against Anthropic, and it does not stand in for "Integration.Claude", the
real-key suite: that one, run with a key that works, is the only thing that
would settle whether Anthropic accepts what shroom sends. This suite exists
because nothing automated had ever driven @baikai-claude@'s wire format at
all, and because a machine can serve its own Anthropic-compatible endpoint
from one NixOS line.

The lenient component is Ollama, not Bifrost. Bifrost passes the schema
through byte-exact; Ollama then enforces it as a grammar, which it has been
measured to stop doing at the second @$ref@ hop, and which it does not do at
all for some string formats. Four asymmetries between what shroom emits,
what Anthropic does with it, and what this stack does with it are known, and
all four go green here while Anthropic answers 400 or enforces something this
stack does not:

+--------------------------------------------------+--------------------+----------------------------------------------------------------------------+
| shroom emits                                     | Anthropic          | this stack                                                                 |
+==================================================+====================+============================================================================+
| object without @additionalProperties: false@     | __400__            | green                                                                      |
+--------------------------------------------------+--------------------+----------------------------------------------------------------------------+
| raw @minimum@\/@maximum@                         | __400__            | green                                                                      |
+--------------------------------------------------+--------------------+----------------------------------------------------------------------------+
| string @format@ for @email@\/@uri@\/@hostname@   | grammar-enforced   | green; llama.cpp leaves these unconstrained                                |
+--------------------------------------------------+--------------------+----------------------------------------------------------------------------+
| mangled @tool_use_id@                            | __400__            | green; nothing in the stack checks the id matches a prior @tool_use@       |
+--------------------------------------------------+--------------------+----------------------------------------------------------------------------+

So a green run here says nothing about any of those four. The bytes shroom
emits are asserted against Anthropic's documented contract by the offline
suite @shroom-test-baikai-wire@ (@test\/WireContract.hs@), which runs in the
default test gate and is where a regression in those four would turn CI red.

= The three failure modes, and which this suite covers

* __Structured output: covered.__ The decisive case, @a schema demanding
  field names the model cannot produce unprompted@, sends a schema whose
  properties are @quexnorb@ and @zblorf@, nested one object deep, and fails
  unless exactly those names come back. It runs with no retries, so a model
  that was told about the mismatch by the retry loop and then corrected
  itself cannot mask a dropped schema. That the request is accepted at all
  is also the live confirmation that shroom's flat @output_config.format@
  shape is one an Anthropic-Messages implementation takes.
* __@tool_use_id@ fidelity: not covered.__ Neither Bifrost nor Ollama checks
  that a @tool_result@'s id matches a preceding @tool_use@'s, so a mangled id
  round-trips green through this stack. The tool-use case here shows only
  that a tool call and its result survive the round trip at all: the model
  is offered a tool, calls it, and the proxy accepts the next request, which
  carries the tool's result, and answers it. That the id is echoed
  byte-identically is asserted offline by @shroom-test-baikai-wire@, which
  is the only place it can be. The case drives 'runBackendChat' by hand with
  an empty schema rather than going through @promptTools@, because Ollama
  applies a request's schema as a grammar to the whole reply, tool call
  included: measured on 2026-10-12 against @llama3.2:1b@ and
  @qwen3:4b-instruct-2507-q4_K_M@, a tool and a real schema in one request
  gets a direct answer, or the call written out as text, where the same
  request with an empty schema gets a @tool_use@ block. shroom always sends
  both together, so what this stack cannot show is a tool call under a live
  schema (the shared case that offers a tool, @conferenceChainWithTools@, is
  among those left out below).
* __The refusal branch: not covered.__ Bifrost in front of Ollama cannot be
  made to produce a @stop_reason: refusal@, so there is nothing to
  exercise it with here. @shroom-test-baikai-wire@ serves a canned refusal
  and asserts both that the backend reads it as a refusal and that a prompt
  reports it once, not as a parse error.

The remaining cases are "Integration.Cases".'backendIntegrationTests',
shared with the other two integration suites.

= From a clean checkout to a green run

__1. Start the proxy.__ Either by the NixOS module,

> services.ollama.shroom.anthropicProxy.enable = true;

which runs @shroom-anthropic-proxy.service@ on @127.0.0.1:8787@ (see
@nix\/ollama-shroom.nix@), or by hand, from a checkout, against an Ollama
already listening on @127.0.0.1:11434@ and already holding the model:

> APP=$(mktemp -d)   # fresh and writable: a stale config.db shadows an edited config.json
> jq -n '{providers:{ollama:{keys:[{name:"ollama-shroom",value:"unused",models:["*"],weight:1,ollama_key_config:{url:"http://127.0.0.1:11434"}}]}}}' > $APP/config.json
> $(nix build .#bifrost --no-link --print-out-paths)/bin/bifrost-http -host 127.0.0.1 -port 8787 -app-dir $APP

Startup takes some seconds; @curl http:\/\/127.0.0.1:8787\/health@ answers
200 when it is ready. The key must carry @"models":["*"]@ or Bifrost
refuses every model. Stop it with @pkill -x bifrost-http@, never @pkill -f@.

__2. Set the environment.__ All three variables are required; the suite
exits non-zero, naming the first one missing, rather than skipping.

* @ANTHROPIC_PROXY_URL@ — the proxy's base URL, including Bifrost's route
  prefix and no trailing @\/v1@ (baikai strips one and appends its own):
  @http:\/\/127.0.0.1:8787\/anthropic@. Bare @\/v1\/messages@ answers 405.
* @ANTHROPIC_PROXY_MODEL@ — the model id with Bifrost's provider prefix,
  e.g. @ollama\/llama3.2:1b@.
* @ANTHROPIC_PROXY_KEY@ — a placeholder credential, e.g. @unused@. Bifrost
  here accepts any (or no) key; the constructor still wants some text.

__3. Run it.__ The suite is behind its own cabal flag, @anthropic-proxy@ (on
by default, like its siblings), and the offline gates never run it:

> ANTHROPIC_PROXY_URL=http://127.0.0.1:8787/anthropic \
> ANTHROPIC_PROXY_MODEL=ollama/llama3.2:1b \
> ANTHROPIC_PROXY_KEY=unused \
>   cabal test shroom-test-integration-anthropic-proxy --enable-tests \
>     --test-options=--timeout=5m --test-option=-p --test-option='!/conferenceChain/'

The @conferenceChain@ cases are left out, as in the Ollama CI job: they test
a model's capability, which a small local model does not have inside any
sensible timeout. Measured on 2026-10-12 through this proxy, both of them
fail or time out on @llama3.2:1b@ and on @qwen3:4b-instruct-2507-q4_K_M@,
while every other case passes.

Because the flag defaults to on, every cabal invocation in
@.github\/workflows\/ci.yml@ that carries a flag set subtracts it
(@-f-anthropic-proxy@). No CI job runs this suite.
-}
module Main (main) where

-- base
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import System.Exit (die)

-- text
import Data.Text (Text, pack)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON, ToJSON, object, (.=))

-- openapi3
import Data.OpenApi (ToSchema)

-- generics-sop
import Generics.SOP qualified as SOP

-- tasty
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)

-- shroom
import Control.Monad.Prompt (Promptable)
import Control.Monad.Prompt.Backend (Backend (..), BackendReply (..), ContextItem (..), ToolCall (..), ToolDef (..), ToolResult (..))
import Control.Monad.Prompt.Baikai (anthropicCompatBackend)
import Control.Monad.Prompt.Effect (PromptConfig (..), defaultPromptConfig)
import Control.Monad.Prompt.Effect qualified as Effect
import Data.Shroom.Class (Describable (..), Surveyable)

-- test
import Integration.Cases (backendIntegrationTests)

main :: IO ()
main = do
  baseUrl <- requireEnv "ANTHROPIC_PROXY_URL" "the proxy's base URL, e.g. http://127.0.0.1:8787/anthropic"
  model <- requireEnv "ANTHROPIC_PROXY_MODEL" "the model id with the provider prefix, e.g. ollama/llama3.2:1b"
  key <- requireEnv "ANTHROPIC_PROXY_KEY" "a placeholder credential, e.g. unused"
  integrationTests baseUrl model key >>= defaultMain

{- | The value of an environment variable, or a loud exit naming it and what
it should hold. Never a silent skip: a suite that exits 0 without having run
would read as a green contract test.
-}
requireEnv :: String -> String -> IO Text
requireEnv name what =
  lookupEnv name >>= \case
    Just value | not (null value) -> pure (pack value)
    _ ->
      die $
        name
          <> " not set ("
          <> what
          <> "); the Anthropic proxy integration suite needs a running proxy, see the module documentation of Integration.AnthropicCompat for how to start one"

integrationTests :: Text -> Text -> Text -> IO TestTree
integrationTests baseUrl model key = do
  backend <- anthropicCompatBackend baseUrl model key
  -- A second one, run in 'IO' directly: 'toolRoundTrip' calls 'runBackendChat' by hand.
  ioBackend <- anthropicCompatBackend baseUrl model key
  pure $
    testGroup
      "Anthropic proxy integration"
      [ backendIntegrationTests backend
      , testCase "a schema demanding field names the model cannot produce unprompted" $ do
          -- No retries: a retry's correction message names the parse error, which
          -- would let a model that never saw the schema find the names anyway.
          result <-
            Effect.runPromptResultEff backend defaultPromptConfig {maxRetries = 0} $ do
              Effect.context "Describe one mineral specimen."
              Effect.prompt @Specimen
          case result of
            Left err -> assertFailure ("the schema's own property names did not come back: " <> T.unpack err)
            -- Decoding already proved the names: the 'FromJSON' instance wants
            -- exactly @quexnorb@ and @zblorf@ inside the nested object.
            Right specimen -> assertBool "the nested probe's text is non-empty" (not (T.null (quexnorb (probe specimen))))
      , testCase "a tool call and its result survive the round trip" (toolRoundTrip ioBackend)
      ]

-- * The decisive schema

{- | The inner object of the decisive case. Its property names are ones no
model would write unprompted, so getting them back is evidence the schema
reached the model's decoder, not that the model guessed.
-}
data Probe = Probe
  { quexnorb :: Text
  , zblorf :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

{- | The outer object, so that 'Probe' sits one object deep and the schema has a
@$ref@ hop to follow rather than a flat list of fields.
-}
newtype Specimen = Specimen {probe :: Probe}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

-- The descriptions deliberately name no property: they are sent to the model.
instance Describable Probe where describeType _ = "A reading taken from a specimen."

instance Surveyable Probe

instance Describable Specimen where describeType _ = "A specimen with one reading."

instance Surveyable Specimen

instance Promptable Specimen

-- * The tool-use case

{- | Offer one tool, expect the model to call it, send its result back, and
expect the proxy to accept that turn and answer it. Driven through
'runBackendChat' by hand with an empty schema rather than through
@promptTools@: see the module documentation for why a real schema would hide
the tool call on this stack. Not a test of @tool_use_id@ fidelity, which
nothing here can check.
-}
toolRoundTrip :: Backend IO -> Assertion
toolRoundTrip backend = do
  let tools =
        [ ToolDef
            { toolDefName = "read_dial"
            , toolDefDescription = "Read the number a named dial currently shows."
            , toolDefSchema =
                object
                  [ "type" .= ("object" :: Text)
                  , "properties" .= object ["dialName" .= object ["type" .= ("string" :: Text)]]
                  , "required" .= (["dialName"] :: [Text])
                  , "additionalProperties" .= False
                  ]
            }
        ]
      conversation = [UserMessage "Use the tool to read the dial called north, then report the number it shows."]
      chat ctx = runBackendChat backend ctx "The number the dial shows." (object []) tools
  first <- chat conversation
  calls <- case first of
    Right (BackendToolCalls cs@(_ : _)) -> pure cs
    Right other -> assertFailure ("expected the model to call the tool, got: " <> show other)
    Left err -> assertFailure (show err)
  let results =
        [ ToolResult {toolResultId = call.toolCallId, toolResultName = call.toolCallName, toolResultOutcome = Right "The dial reads 7321."}
        | call <- calls
        ]
  second <- chat (conversation <> [ToolCallMessage calls, ToolResultMessage results])
  -- Any reply will do: that the proxy accepted a request carrying a tool_result
  -- and answered it is the whole claim. Whether the model then answers or asks
  -- for the tool again is the model's business.
  either (assertFailure . show) (const (pure ())) second
