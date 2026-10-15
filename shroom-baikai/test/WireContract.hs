{-# LANGUAGE DeriveAnyClass #-}

{- | The bytes @shroom-baikai@ sends to an Anthropic-compatible host, asserted
against Anthropic's documented contract, with no server and no model.

'anthropicCompatBackend' is pointed at a listener on @127.0.0.1@ that this
process runs itself: it records the request, answers with a canned Anthropic
streaming reply, and the assertions read the recording. Nothing leaves the
machine, so this suite runs in the default test gate.

= Why it exists

Four things are known to be Anthropic-strict and Ollama-lenient: an object
without @additionalProperties: false@, raw @minimum@\/@maximum@, a string
@format@ that llama.cpp leaves unconstrained, and a @tool_use_id@ not echoed
back. A live run against a lenient host goes green on all four, and the code
that produces the first three, "Control.Monad.Prompt.Schema", is slated for a
wholesale rewrite. This suite is what would notice that rewrite going wrong.
The fourth, the @tool_use_id@, and the refusal branch are the /response/ leg:
the listener serves a sequence of canned replies, so a tool loop runs end to
end offline, and what shroom sends after a @tool_use@ and what it makes of a
refusal are asserted (see "The response leg").

= How to read a failure

__A failure is fixed in the emitter, not in the expectation.__ Every assertion
below is a citation of Anthropic's documentation, and the citation carries the
date it was read. Changing an expectation means overruling a citation: re-read
the source, and update the citation (text and date) in the same revision.
Never loosen an assertion to make it pass.

= The accepted schema keywords

The allowlist of schema keywords Anthropic's structured outputs accept, the
checks built on it, and the authorities [S1]-[S4] for each (all read on
2026-10-12) are in "SchemaCheck", which holds the pure checker and the
fixtures that prove it can fail; the assertions below cite them by those
labels. The loopback listener that records the requests is "Loopback".

= The response leg

The listener replies in order, one canned reply per request, and records
every request, so a conversation of several turns can be driven and the later
requests read. Further authority, all read on 2026-10-12:

[S5] Anthropic, "Handle tool calls"
(@platform.claude.com\/docs\/en\/agents-and-tools\/tool-use\/handle-tool-calls@),
"Handling results from client tools": a @tool_use@ block's @id@, @name@ and
@input@; a @tool_result@ block's @tool_use_id@ ("The @id@ of the tool use
request this is a result for"), @content@ and @is_error@; and the formatting
requirements (a @tool_result@ is sent in a @user@ message that immediately
follows the message holding its @tool_use@).

[S6] Claude Code, "Error reference"
(@code.claude.com\/docs\/en\/errors@), section "Tool use or thinking block
mismatch": lists @API Error: 400 orphaned tool_result in conversation
history@ and @API Error: 400 ... unexpected `tool_use_id` found in
`tool_result` blocks@, and says all variants mean "the sequence of
@tool_use@, @tool_result@, and @thinking@ blocks in history no longer matches
what the API expects". This, and not the Messages API reference (which says
the id must match and not what happens when it does not), is the documented
behaviour for a @tool_result@ whose id matches no @tool_use@. What that page
does not give is the API's own error type and wording; those are carried in
'toolUseIdRoundTrips' as observed, and not re-observed here.

[S7] Anthropic, "Handling stop reasons"
(@platform.claude.com\/docs\/en\/build-with-claude\/handling-stop-reasons@),
@refusal@ and "Streaming considerations"; and "Refusals and fallback"
(@platform.claude.com\/docs\/en\/build-with-claude\/refusals-and-fallback@),
"What a refusal looks like".

[S8] Anthropic, "Streaming Messages"
(@platform.claude.com\/docs\/en\/build-with-claude\/streaming@), the event
flow and the example of a streaming request with tool use (@tool_use@ block
with an empty @input@, then @input_json_delta@ events of @partial_json@, then
@message_delta@ with @stop_reason: tool_use@).

= The envelope, observed

What @baikai-claude@'s @AnthropicMessages@ transport makes of
@responseFormat = Just (JsonSchema ((jsonSchemaFormat \"response\" schema)
{strict = True}))@ was not known before this suite. Captured on the wire on
2026-10-12, for a record of a @Text@ and an @Int@ (the @Int@'s description
abridged):

> "output_config":{"format":{"schema":{"additionalProperties":false,
>   "properties":{"quexnorb":{"type":"string"},"zblorf":{"description":"Must be
>   between ...","type":"integer"}},"required":["quexnorb","zblorf"],
>   "type":"object"},"type":"json_schema"}}

That is [S2]'s flat shape: @type@ and @schema@ are siblings inside
@output_config.format@. @name@ (which 'jsonSchemaFormat' is given) and
@strict@ are not serialised; the request also has @stream: true@ and no
top-level @response_format@.

= Known gaps the test type avoids

The emitter currently violates the accepted set for four shapes, so the
test type 'Gadget' deliberately contains none of them, and the suite is green
on a state that is not yet correct:

* a number's @format@: every 'Double' (and 'Float') goes out as
  @{\"type\":\"number\", \"format\":\"double\"}@, and [S1]\/[S4] list @format@
  for strings only;
* a constructor with two or more fields, which goes out as a tuple: @items@
  as an array, @maxItems: 2@ and @minItems: 2@, against [S1]'s "Array
  constraints beyond @minItems@ of 0 or 1" and [S4]'s dropping of @maxItems@;
* a 'Char', which @openapi3@ renders with @minLength@ and @maxLength@, neither
  of which is in 'allowedKeywords';
* a 'Natural', which carries @exclusiveMinimum@ ('numericBounds' names it, and
  it is no more in 'allowedKeywords' than the others).

The last two are the ones @Control.Monad.Prompt.Promptable@'s own Haddock
("Schema keywords Anthropic rejects") records for the base types, whose
'Promptable' instances pass the element's schema through unchanged, so a field
or a container of one inherits them.

Nor does this suite drive the per-field schemas of the @Generically@ strategy
(@deriving via Generically@, see @Control.Monad.Prompt.Generic@) through the
allowlist: it prompts for a 'Gadget' flat, which never runs that strategy, so
the several smaller requests the strategy sends, each with its own schema, are
unchecked.

These are recorded here, not fixed: the emitter is not changed by this suite.
They are tracked in issue #4, "Arc B — one definition yielding schema and
codec together", where the schema and the codec come from one definition.

'checkerRejects' proves the checks do flag the first two shapes, so the day
'Gadget' gains a 'Double' the failure will be real. It carries no fixture for
a 'Char' or a 'Natural'; the checks would flag their keywords all the same, as
'allowedKeywords' and 'numericBounds' stand.
-}
module Main (main) where

-- base
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Maybe (fromMaybe, isJust)
import GHC.Generics (Generic)
import System.Timeout (timeout)

-- bytestring
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL

-- text
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

-- aeson
import Data.Aeson (FromJSON, ToJSON, Value (..), decodeStrict, encode, object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap

-- vector
import Data.Vector qualified as V

-- openapi3
import Data.OpenApi (ToSchema)

-- generics-sop
import Generics.SOP qualified as SOP

-- tasty
import Test.Tasty (TestTree, defaultMain, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase, (@?=))

-- shroom
import Control.Monad.Prompt (Promptable)
import Control.Monad.Prompt.Backend (Backend (..), BackendError (..), BackendReply, ContextItem (..))
import Control.Monad.Prompt.Baikai (anthropicCompatBackend)
import Control.Monad.Prompt.Effect (PromptConfig (..), defaultPromptConfig)
import Control.Monad.Prompt.Effect qualified as Effect
import Control.Monad.Prompt.Tool (ToolHandler (..), Toolable, runTool, toolBinding)
import Data.Shroom.Class (Describable (..), Surveyable)

-- test modules
import Loopback (Captured (..), withCannedServer)
import SchemaCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "shroom-baikai wire contract"
    [ withResource capture (\_ -> pure ()) $ \getExchange ->
        testGroup
          "the request sent to an Anthropic-compatible host"
          [ testCase "the exchange completes: one POST, the canned answer read back" (exchangeCompletes getExchange)
          , testCase "it is POST /v1/messages with the documented headers" (requestLineAndHeaders getExchange)
          , testCase "it asks for structured output in output_config.format" (envelopePresent getExchange)
          , testCase "output_config.format is the flat json_schema envelope" (envelopeIsFlat getExchange)
          , testCase "the schema carries the type's own property names and required list" (schemaCarriesTheType getExchange)
          , testCase "no keyword outside Anthropic's accepted set appears in the schema" (schemaUsesAllowedKeywords getExchange)
          , testCase "additionalProperties is false on every object" (everyObjectIsClosed getExchange)
          , testCase "no minimum or maximum survives" (noNumericBounds getExchange)
          , testCase "no external $ref, and every $ref resolves" (noExternalRefs getExchange)
          , testCase "no recursion" (noRecursion getExchange)
          , testCase "the remaining documented constraints hold (format, minItems, enum, items, allOf)" (otherConstraintsHold getExchange)
          ]
    , withResource captureToolLoop (\_ -> pure ()) $ \getExchange ->
        testGroup
          "a tool_use reply, and what is sent back"
          [ testCase "the loop completes: two POSTs, the tool run once on the canned input, the answer read back" (toolLoopCompletes getExchange)
          , testCase "the first request offers the tool as a name and an input_schema" (toolIsOffered getExchange)
          , testCase "the second request replays the tool_use under the id it arrived with" (toolUseIsReplayed getExchange)
          , testCase "the tool_result's tool_use_id is byte-identical to the tool_use block's id" (toolUseIdRoundTrips getExchange)
          , testCase "the tool_result immediately follows the tool_use and carries the tool's output" (toolResultFollowsToolUse getExchange)
          ]
    , testGroup
        "a refusal reply"
        [ withResource captureRefusalDirect (\_ -> pure ()) $ \getExchange ->
            testCase "the backend reads stop_reason refusal back as BackendRefusal" (refusalIsRefusal getExchange)
        , withResource captureRefusalFrontDoor (\_ -> pure ()) $ \getExchange ->
            testCase "a prompt reports it as a refusal, once, and not as a parse error" (refusalIsTerminal getExchange)
        ]
    , checkerRejects
    ]

-- * The test type

{- | A nested record, used both as a field and as the element of a list, so the
schema has two hops of nesting — the shape the library's own traffic has, and
the one a flat test type would not cover.
-}
data Inner = Inner {snarfle :: Text, glorp :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

-- | A plain enumeration.
data Colour = Red | Green
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

{- | A sum with a non-nullary constructor, which @openapi3@ renders as @oneOf@
and the emitter has to turn into @anyOf@. Its one-field constructor is
deliberate; see "Known gaps the test type avoids" in the module header.
-}
data Variant = Plain | Wrapped Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

{- | The type the request is for. The property names are ones nobody would
guess, so finding them in the emitted schema is evidence the type's own
schema was emitted and not some other. @wibble@ is the one optional field, so
the @required@ list is not simply every property. An 'Int' is in there to
carry the @minimum@\/@maximum@ @openapi3@ derives for it.
-}
data Gadget = Gadget
  { quexnorb :: Text
  , zblorf :: Inner
  , frumps :: [Inner]
  , wibble :: Maybe Text
  , hue :: Colour
  , variant :: Variant
  , tags :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

instance Describable Inner where describeType _ = "An inner record."

instance Surveyable Inner

instance Describable Colour where describeType _ = "A colour."

instance Surveyable Colour

instance Describable Variant where describeType _ = "A variant."

instance Surveyable Variant

instance Describable Gadget where describeType _ = "A gadget."

instance Surveyable Gadget

instance Promptable Gadget

-- | What the canned reply says: a valid 'Gadget', so the call completes.
cannedGadget :: Gadget
cannedGadget =
  Gadget
    { quexnorb = "q"
    , zblorf = Inner {snarfle = "s", glorp = 1}
    , frumps = [Inner {snarfle = "t", glorp = 2}]
    , wibble = Nothing
    , hue = Green
    , variant = Wrapped "w"
    , tags = ["x"]
    }

{- | The tool the canned @tool_use@ block calls. Its derived name,
@frobnicate_widget@, is nothing like the id it is called under, so a name in
the id's place (or the id in the name's) cannot pass for the right one.
-}
newtype FrobnicateWidget = FrobnicateWidget {serial :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

instance Describable FrobnicateWidget where describeType _ = "Frobnicate a widget."

instance Surveyable FrobnicateWidget

instance Toolable FrobnicateWidget

{- | The id of the canned @tool_use@ block, in the shape Anthropic's own
examples use (a @toolu_@ prefix and mixed-case alphanumerics), so a
lower-casing or a re-prefixing would show.
-}
cannedToolUseId :: Text
cannedToolUseId = "toolu_01T1x1fJ34qAmk2tNTrN7Up6"

-- | The name the canned @tool_use@ block calls, as 'FrobnicateWidget' derives it.
cannedToolName :: Text
cannedToolName = "frobnicate_widget"

-- | The input the canned @tool_use@ block carries.
cannedToolInput :: FrobnicateWidget
cannedToolInput = FrobnicateWidget {serial = "SN-0042"}

-- | What the tool answers with.
toolOutput :: Text
toolOutput = "widget SN-0042 frobnicated"

-- * The exchange

-- | What came of one run of the library against the listener.
data Exchange r = Exchange
  { result :: r
  -- ^ What the run produced.
  , requests :: [Captured]
  -- ^ Every request the listener received, in order.
  }

-- | The exchange of a plain @prompt@ for a 'Gadget'.
type GadgetExchange = Exchange (Either Text Gadget)

{- | Run an action against a fresh listener that serves these replies in
order. The action is given the base URL to point a backend at. It is bounded
to 10s, and what it returned is paired with everything the listener recorded.
-}
against :: [ByteString] -> (Text -> IO r) -> IO (Exchange r)
against replies act =
  withCannedServer replies $ \port recorded -> do
    outcome <- timeout 10_000_000 (act ("http://127.0.0.1:" <> T.pack (show port)))
    recorded' <- recorded
    case outcome of
      Nothing -> assertFailure "the call did not finish within 10s"
      Just r -> pure (Exchange r recorded')

-- | A backend aimed at the listener at this base URL, through the library's own front door.
mkBackend :: (MonadIO m) => Text -> IO (Backend m)
mkBackend baseUrl = anthropicCompatBackend baseUrl "wire-contract-model" "wire-contract-key"

{- | Prompt for a 'Gadget' through the library's own front door
('Effect.prompt', not a hand-built request), against a listener that answers
once with 'cannedGadget'. Runs once; every assertion reads the same capture.
-}
capture :: IO GadgetExchange
capture =
  against [anthropicTextReply (encodeText cannedGadget)] $ \url -> do
    backend <- mkBackend url
    Effect.runPromptResultEff backend defaultPromptConfig {maxRetries = 0} (Effect.prompt @Gadget)

-- | A value as the JSON text a model would answer with.
encodeText :: (ToJSON a) => a -> Text
encodeText = decodeUtf8 . BL.toStrict . encode

-- | The single request of the exchange, or a failure saying how many there were.
theRequest :: IO (Exchange r) -> IO Captured
theRequest getExchange = do
  ex <- getExchange
  case ex.requests of
    [r] -> pure r
    rs -> assertFailure ("expected exactly one request, got " <> show (length rs))

-- | The request body, decoded to a JSON object.
requestBody :: IO (Exchange r) -> IO Value
requestBody getExchange = theRequest getExchange >>= bodyObject

-- | A request's body, decoded to a JSON object.
bodyObject :: Captured -> IO Value
bodyObject r = case decodeStrict r.body of
  Just v@(Object _) -> pure v
  other -> assertFailure ("the request body is not a JSON object: " <> show other <> " from " <> show r.body)

-- | The @output_config.format@ object of the request, or a failure saying it is absent.
outputConfigFormat :: IO GadgetExchange -> IO (KeyMap.KeyMap Value)
outputConfigFormat getExchange = do
  b <- requestBody getExchange
  case b of
    Object o
      | Just (Object cfg) <- KeyMap.lookup "output_config" o
      , Just (Object fmt) <- KeyMap.lookup "format" cfg ->
          pure fmt
    _ ->
      assertFailure
        ( "the request has no output_config.format: the emitter sends no structured-output request at all. Body: "
            <> show b
        )

-- | The emitted schema: @output_config.format.schema@.
emittedSchema :: IO GadgetExchange -> IO Value
emittedSchema getExchange = do
  fmt <- outputConfigFormat getExchange
  case KeyMap.lookup "schema" fmt of
    Just s@(Object _) -> pure s
    other -> assertFailure ("output_config.format.schema is not a JSON object: " <> show other)

{- | The call completed: one request was made, and the canned answer was
parsed back into the 'Gadget' it said. A guard on the harness: were it to
fail, every other assertion would be reading the wrong thing.
-}
exchangeCompletes :: IO GadgetExchange -> IO ()
exchangeCompletes getExchange = do
  ex <- getExchange
  length ex.requests @?= 1
  ex.result @?= Right cannedGadget

-- * Assertions on the request

{- | The request is @POST \/v1\/messages@ and carries the headers the API
overview lists as required: @anthropic-version@ and @content-type:
application\/json@, plus a credential (@x-api-key@, which the same table
calls a legacy fallback for @Authorization@ that is "still supported"; this
adapter sends the former).

Source: [S3] "Available APIs" and "Authentication". Read 2026-10-12.
-}
requestLineAndHeaders :: IO GadgetExchange -> IO ()
requestLineAndHeaders getExchange = do
  r <- theRequest getExchange
  assertBool ("not POST /v1/messages: " <> show r.requestLine) ("POST /v1/messages " `BS.isPrefixOf` r.requestLine)
  assertBool "no anthropic-version header" (isJust (lookup "anthropic-version" r.headers))
  lookup "content-type" r.headers @?= Just "application/json"
  let credentialHeaders = ["x-api-key", "authorization"] :: [ByteString]
  assertBool "no x-api-key or authorization header" (any (`elem` map fst r.headers) credentialHeaders)

{- | The request asks for structured output at all: @output_config.format@ is
present. An emitter that sends no schema fails here — a dropped schema
raises nothing at runtime, it returns plausible prose.

Source: [S2], the @output_config.format@ request shape. Read 2026-10-12.
-}
envelopePresent :: IO GadgetExchange -> IO ()
envelopePresent getExchange = void (outputConfigFormat getExchange)

{- | The envelope has the shape observed on the wire and shown in the module
header, which is [S2]'s: @type: "json_schema"@ and @schema@ as the only
members of @output_config.format@ — no @name@, no @strict@ — and no OpenAI
style top-level @response_format@.

Source: [S2]. Read 2026-10-12. The observed shape is not an extra
requirement over [S2]; it is what was recorded, and a change to it is a
change to review against [S2].
-}
envelopeIsFlat :: IO GadgetExchange -> IO ()
envelopeIsFlat getExchange = do
  fmt <- outputConfigFormat getExchange
  sort (Key.toText <$> KeyMap.keys fmt) @?= ["schema", "type"]
  KeyMap.lookup "type" fmt @?= Just "json_schema"
  b <- requestBody getExchange
  case b of
    Object o -> assertBool "a top-level response_format is present" (not (KeyMap.member "response_format" o))
    _ -> pure ()

{- | The schema is the type's own: the root carries 'Gadget'\'s property
names and its @required@ list (the optional @wibble@ among the former, not
the latter), and so do the two nested records, whether the schema inlines
them or reaches them through @$ref@\/@$defs@.

Source: [S1] "Supported features" (@required@, @$ref@\/@$defs@). Read
2026-10-12.
-}
schemaCarriesTheType :: IO GadgetExchange -> IO ()
schemaCarriesTheType getExchange = do
  root <- emittedSchema getExchange
  let at ctx path = maybe (assertFailure (ctx <> ": no such node " <> show path)) pure (nodeAt root path)
  gadget <- at "root" []
  zblorfNode <- at "zblorf" ["zblorf"]
  frumpsItems <- at "frumps[]" ["frumps", "[]"]
  propertyNames gadget @?= sort ["quexnorb", "zblorf", "frumps", "wibble", "hue", "variant", "tags"]
  requiredNames gadget @?= sort ["quexnorb", "zblorf", "frumps", "hue", "variant", "tags"]
  forM_ ([("zblorf", zblorfNode), ("frumps[]", frumpsItems)] :: [(Text, Value)]) $ \(ctx, node) -> do
    (ctx, propertyNames node) @?= (ctx, ["glorp", "snarfle"])
    (ctx, requiredNames node) @?= (ctx, ["glorp", "snarfle"])

{- | No keyword outside the accepted list ('allowedKeywords') appears anywhere
in the emitted schema — at the root, in a property, an array's @items@, an
@anyOf@\/@allOf@ alternative, a @$defs@ entry. An allowlist rather than a
list of known offenders: a keyword nobody thought of is caught too.

Source: [S1] "Supported features" and "Not supported"; [S4] for @title@.
Read 2026-10-12.
-}
schemaUsesAllowedKeywords :: IO GadgetExchange -> IO ()
schemaUsesAllowedKeywords getExchange = emittedSchema getExchange >>= expectNone . unsupportedKeywords

{- | @additionalProperties: false@ on every object — a node that is of
@type: object@ or has @properties@ — and never anything but @false@.

Source: [S1] "Supported features" (@additionalProperties@ "must be set to
@false@ for objects") and "Not supported". Read 2026-10-12.
-}
everyObjectIsClosed :: IO GadgetExchange -> IO ()
everyObjectIsClosed getExchange = emittedSchema getExchange >>= expectNone . objectsNotClosed

{- | No @minimum@, @maximum@ or @multipleOf@ survives anywhere (the emitter
folds a bound into the field's @description@ instead, which [S1]'s SDK
paragraph describes).

Source: [S1] "Not supported" (numerical constraints, such as @minimum@,
@maximum@, @multipleOf@). Read 2026-10-12.
-}
noNumericBounds :: IO GadgetExchange -> IO ()
noNumericBounds getExchange = emittedSchema getExchange >>= expectNone . numericBounds

{- | Every @$ref@ is internal (starts @#\/@) and resolves to a node in the
schema. Today's emitter inlines everything, so there are none; the assertion
is on the contract, so that a rewrite that does emit @$defs@ is held to it.

Source: [S1] "Supported features" (@$ref@, @$defs@ and @definitions@;
"external @$ref@ not supported"). Read 2026-10-12.
-}
noExternalRefs :: IO GadgetExchange -> IO ()
noExternalRefs getExchange = emittedSchema getExchange >>= expectNone . badRefs

{- | No recursion: no @$ref@ leads, through other @$ref@s, back to itself.
Today's emitter caps its own inlining at 20 levels, which for a recursive
type would leave a truncated schema rather than a cycle; this guards the day
it emits @$defs@ instead.

Source: [S1] "Not supported" (recursive schemas). Read 2026-10-12.
-}
noRecursion :: IO GadgetExchange -> IO ()
noRecursion getExchange = emittedSchema getExchange >>= expectNone . recursion

{- | The remaining documented per-keyword constraints: @format@ only on a
string and only from the supported list; @minItems@ only 0 or 1; @enum@
holding no arrays or objects; @items@ one schema, not a tuple; no @$ref@
under @allOf@.

Source: [S1] "Supported features" (the @enum@, @allOf@, string-format and
@minItems@ lines) and "Not supported" (array constraints); [S4] for a
number's @format@. Read 2026-10-12.
-}
otherConstraintsHold :: IO GadgetExchange -> IO ()
otherConstraintsHold getExchange = emittedSchema getExchange >>= expectNone . otherConstraints

expectNone :: [Text] -> IO ()
expectNone [] = pure ()
expectNone vs = assertFailure (T.unpack (T.unlines ("the emitted schema violates Anthropic's documented subset:" : vs)))

-- * Assertions on a tool loop

-- | What a tool-driven prompt came to: its result, and each input the tool was run on.
data ToolRun = ToolRun
  { answered :: Either Text Gadget
  , dispatched :: [FrobnicateWidget]
  }

{- | Prompt for a 'Gadget' with 'FrobnicateWidget' on offer, through the
library's own front door ('Effect.promptTools' and 'toolBinding'), against a
listener whose first reply is a @tool_use@ for it and whose second is the
final answer. The tool's handler records what it was run on.
-}
captureToolLoop :: IO (Exchange ToolRun)
captureToolLoop =
  against
    [ anthropicToolUseReply cannedToolUseId cannedToolName (toJSON cannedToolInput)
    , anthropicTextReply (encodeText cannedGadget)
    ]
    $ \url -> do
      backend <- mkBackend url
      ran <- newIORef []
      let handler = ToolHandler $ \t -> modifyIORef' ran (<> [t]) >> pure (Right toolOutput)
      answer <-
        Effect.runPromptResultEff backend defaultPromptConfig {maxRetries = 0} $
          runTool handler (Effect.promptTools @Gadget [toolBinding @FrobnicateWidget])
      ToolRun answer <$> readIORef ran

-- | One message of a request, as its role and its content blocks (none for a plain-text content).
data Turn = Turn
  { role :: Text
  , blocks :: [KeyMap.KeyMap Value]
  }

-- | The @messages@ of a request, in order.
turns :: Captured -> IO [Turn]
turns r = do
  b <- bodyObject r
  case b of
    Object o | Just (Array ms) <- KeyMap.lookup "messages" o -> traverse turn (V.toList ms)
    _ -> assertFailure ("the request has no messages array: " <> show b)
  where
    turn (Object m)
      | Just (String ro) <- KeyMap.lookup "role" m =
          pure Turn {role = ro, blocks = case KeyMap.lookup "content" m of Just (Array bs) -> [blk | Object blk <- V.toList bs]; _ -> []}
    turn other = assertFailure ("a message that is not an object with a role: " <> show other)

-- | The blocks of this @type@ across all turns, with the role of the turn each is in.
blocksOfType :: Text -> [Turn] -> [(Text, KeyMap.KeyMap Value)]
blocksOfType t ts = [(turn.role, b) | turn <- ts, b <- turn.blocks, KeyMap.lookup "type" b == Just (String t)]

-- | The second request of a tool loop, or a failure saying how many there were.
secondRequest :: IO (Exchange r) -> IO Captured
secondRequest getExchange = do
  ex <- getExchange
  case ex.requests of
    [_, r] -> pure r
    rs -> assertFailure ("expected exactly two requests, got " <> show (length rs))

{- | The harness's own guard for the tool loop: two requests were made, the
tool ran exactly once on the input the canned block carried (so the
@input_json_delta@ chunks were put back together), and the second reply was
read back as the answer. Were it to fail, the assertions below would be
reading a loop that did not happen.

Source: [S8], the tool-use streaming example (read 2026-10-12) for the
events served; [S5] for the loop itself.
-}
toolLoopCompletes :: IO (Exchange ToolRun) -> IO ()
toolLoopCompletes getExchange = do
  ex <- getExchange
  length ex.requests @?= 2
  ex.result.dispatched @?= [cannedToolInput]
  ex.result.answered @?= Right cannedGadget

{- | The first request offers the tool under its name with an @input_schema@
that is a JSON Schema object carrying the tool's own property. Not the
point of this suite, but the loop cannot start without it.

Source: [S5] "Handle tool calls" (the @tool_use@ block's @name@ and its
@input@ "conforming to the tool's @input_schema@"); the tool definition's
@name@ and @input_schema@ fields are listed on "Define tools"
(@platform.claude.com\/docs\/en\/agents-and-tools\/tool-use\/define-tools@).
Read 2026-10-12.
-}
toolIsOffered :: IO (Exchange ToolRun) -> IO ()
toolIsOffered getExchange = do
  ex <- getExchange
  case ex.requests of
    first : _ -> do
      b <- bodyObject first
      case b of
        Object o | Just (Array tools) <- KeyMap.lookup "tools" o -> case V.toList tools of
          [Object t] -> do
            KeyMap.lookup "name" t @?= Just (String cannedToolName)
            case KeyMap.lookup "input_schema" t of
              Just schema@(Object _) -> propertyNames schema @?= ["serial"]
              other -> assertFailure ("the tool has no input_schema object: " <> show other)
          other -> assertFailure ("expected exactly one object in tools: " <> show other)
        _ -> assertFailure ("the first request has no tools array: " <> show b)
    [] -> assertFailure "no request was made"

{- | The assistant turn of the second request replays the @tool_use@ block
under the id and name it arrived with, and the input it carried. This is
the other half of the pairing: a @tool_result@ can only match a @tool_use@
that the history still contains, under the same id.

Source: [S5] "Handle tool calls" ("Tool result blocks must immediately follow
their corresponding tool use blocks in the message history"). Read
2026-10-12.
-}
toolUseIsReplayed :: IO (Exchange ToolRun) -> IO ()
toolUseIsReplayed getExchange = do
  ts <- secondRequest getExchange >>= turns
  case blocksOfType "tool_use" ts of
    [(ro, b)] -> do
      ro @?= "assistant"
      assertEqual "tool_use.id" (Just (String cannedToolUseId)) (KeyMap.lookup "id" b)
      assertEqual "tool_use.name" (Just (String cannedToolName)) (KeyMap.lookup "name" b)
      assertEqual "tool_use.input" (Just (toJSON cannedToolInput)) (KeyMap.lookup "input" b)
    other -> assertFailure ("expected one tool_use block in the replayed history, got " <> show (length other))

{- | __The @tool_use_id@ round trip.__ The one @tool_result@ in the second
request carries a @tool_use_id@ byte-identical to the @id@ of the @tool_use@
block that was served: compared as decoded text, and again as the bytes
@"tool_use_id":"…"@ in the request body, so no escaping or re-encoding can
hide a difference.

Why this is asserted here and nowhere else: a live host that does not check
a @tool_result@'s id against a preceding @tool_use@'s (a local Ollama, say;
the arc plan's claim, not measured here)
accepts a wrong id, so a mangled one
round-trips green through every live suite and fails only against
Anthropic. The two ids are adjacent 'Text' fields of
'Control.Monad.Prompt.Backend.ToolResult', so transposing them
('toolResultId' and 'toolResultName' in
'Control.Monad.Prompt.Baikai.toBaikaiToolResultMessage') typechecks. Doing so
puts the tool name, @frobnicate_widget@, in this field.

Source, documented: [S5] "Handle tool calls", where @tool_use_id@ is "the
@id@ of the tool use request this is a result for", and [S6], Claude
Code's error reference, "Tool use or thinking block mismatch", which lists
@API Error: 400 orphaned tool_result in conversation history@ (and
@API Error: 400 ... unexpected `tool_use_id` found in `tool_result` blocks@),
and says of all of them that "the sequence of @tool_use@, @tool_result@, and
@thinking@ blocks in history no longer matches what the API expects". An
orphaned @tool_result@ is what a transposed id is. Read 2026-10-12.

Observed, not documented: that the API's answer is a @400@ whose error
type is @invalid_request_error@ and what its message says. [S6] does not
state either verbatim (the wording above is Claude Code's, not the API's),
and this suite does not depend on them: it asserts the id, which is the
documented requirement. (The error type and wording were not re-observed
for this suite.)
-}
toolUseIdRoundTrips :: IO (Exchange ToolRun) -> IO ()
toolUseIdRoundTrips getExchange = do
  r <- secondRequest getExchange
  ts <- turns r
  case blocksOfType "tool_result" ts of
    [(_, b)] -> do
      assertEqual
        "tool_result.tool_use_id is not the id of the tool_use block that was served; Anthropic rejects such a history (S6: 400 orphaned tool_result in conversation history)"
        (Just (String cannedToolUseId))
        (KeyMap.lookup "tool_use_id" b)
      assertBool
        ("the request body does not contain the id verbatim as a tool_use_id: " <> show r.body)
        (("\"tool_use_id\":\"" <> encodeUtf8 cannedToolUseId <> "\"") `BS.isInfixOf` r.body)
    other -> assertFailure ("expected exactly one tool_result block, got " <> show (length other))

{- | The @tool_result@ is in the user turn immediately after the assistant
turn holding its @tool_use@, is not marked as an error, and carries what the
tool answered.

Source: [S5] "Handle tool calls" (a @tool_result@ block is sent in a message
with the @role@ of @user@, must immediately follow its @tool_use@ block's
message, and has an optional @content@ and an optional @is_error@). Read
2026-10-12.
-}
toolResultFollowsToolUse :: IO (Exchange ToolRun) -> IO ()
toolResultFollowsToolUse getExchange = do
  ts <- secondRequest getExchange >>= turns
  case break (hasBlockOfType "tool_use") ts of
    (_, useTurn : next : _) -> do
      useTurn.role @?= "assistant"
      next.role @?= "user"
      case [b | b <- next.blocks, KeyMap.lookup "type" b == Just "tool_result"] of
        [b] -> do
          assertBool "the tool_result is marked is_error" (KeyMap.lookup "is_error" b `elem` ([Nothing, Just (Bool False)] :: [Maybe Value]))
          assertBool
            ("the tool_result does not carry the tool's output: " <> show (KeyMap.lookup "content" b))
            (toolOutput `T.isInfixOf` showValue (fromMaybe Null (KeyMap.lookup "content" b)))
        other -> assertFailure ("expected one tool_result in the turn after the tool_use, got " <> show (length other))
    _ -> assertFailure "no user turn directly after the assistant's tool_use turn"

-- | Whether a turn holds a content block of this @type@.
hasBlockOfType :: Text -> Turn -> Bool
hasBlockOfType t turn = any (\b -> KeyMap.lookup "type" b == Just (String t)) turn.blocks

-- * Assertions on a refusal

{- | A listener whose only reply is a refusal, and a 'Backend' called
directly: no retry loop between the reply and what this returns.
-}
captureRefusalDirect :: IO (Exchange (Either BackendError BackendReply))
captureRefusalDirect =
  against [anthropicRefusalReply] $ \url -> do
    (backend :: Backend IO) <- mkBackend url
    runBackendChat backend [UserMessage "Make a gadget."] "A gadget." (object ["type" .= ("object" :: Text), "additionalProperties" .= False]) []

{- | The same refusal, through 'Effect.prompt' with retries to spare, so a
refusal wrongly treated as retryable would show as extra requests.
-}
captureRefusalFrontDoor :: IO GadgetExchange
captureRefusalFrontDoor =
  against [anthropicRefusalReply] $ \url -> do
    backend <- mkBackend url
    Effect.runPromptResultEff backend defaultPromptConfig {maxRetries = 2} (Effect.prompt @Gadget)

{- | A reply with @stop_reason: "refusal"@ is read back as 'BackendRefusal' —
not as 'BackendTransportError' (a decode failure would be one), not as an
answer, not as 'BackendTruncated'. The category the reply names comes through
in 'refusalCategory', and the message says it was a refusal.

Source: [S7] "Handling stop reasons", @refusal@: "Claude declined to
generate a response. Safety classifiers return this stop reason as a normal
HTTP 200 response, not an error", and "Refusals and fallback", "What a
refusal looks like" (@stop_details@ with @type@, @category@ and
@explanation@; @content@ empty). Read 2026-10-12. Streaming, per [S7]'s
"Streaming considerations": the @stop_reason@ is in the @message_delta@. Not
documented for the stream, and so an assumption about Anthropic's wire taken
from @baikai-claude@'s own reading of it (@MessageDelta.stop_details@): that
@stop_details@ rides in that same @message_delta@. If that is wrong the
@refusalCategory@ line below is wrong, not the 'BackendRefusal' one.
-}
refusalIsRefusal :: IO (Exchange (Either BackendError BackendReply)) -> IO ()
refusalIsRefusal getExchange = do
  ex <- getExchange
  case ex.result of
    Left (BackendRefusal cat msg) -> do
      cat @?= Just "cyber"
      assertBool ("the refusal's message does not say so: " <> show msg) ("refus" `T.isInfixOf` T.toLower msg)
    other -> assertFailure ("expected a BackendRefusal, got " <> show other)

{- | Through 'Effect.prompt' a refusal ends the call at once: one request
although two retries were allowed (a refusal "is not something a re-prompt
fixes"), and the failure text is the refusal's, not a JSON decode error.

Source: [S7] as for 'refusalIsRefusal'; the retry behaviour is
'Control.Monad.Prompt.Effect.runPrompt' own documented contract ("A
'BackendRefusal' is terminal and consumes no retry budget"), not
Anthropic's. Read 2026-10-12.
-}
refusalIsTerminal :: IO GadgetExchange -> IO ()
refusalIsTerminal getExchange = do
  ex <- getExchange
  length ex.requests @?= 1
  case ex.result of
    Left msg -> do
      assertBool ("reported as a parse error: " <> show msg) (not ("decode" `T.isInfixOf` T.toLower msg || "parse" `T.isInfixOf` T.toLower msg))
      assertBool ("the failure does not say it was a refusal: " <> show msg) ("refus" `T.isInfixOf` T.toLower msg)
    Right g -> assertFailure ("a refusal was read as the answer " <> show g)

-- * The checks, checked

{- | The checks must be able to fail. Each schema here breaks one documented
constraint, and the combined check must say so; the last one is a schema that
uses everything the contract allows, including @$defs@ and a non-recursive
@$ref@, and must come back clean. Without this a check that never fires would
look exactly like an emitter that never errs.
-}
checkerRejects :: TestTree
checkerRejects =
  testGroup
    "the checks reject what the contract forbids"
    ( [ testCase ("flags: " <> T.unpack what) $ do
          let vs = allViolations schema
          assertBool ("expected a violation mentioning " <> show needle <> ", got " <> show vs) (any (needle `T.isInfixOf`) vs)
      | (what, needle, schema) <- badSchemas
      ]
        <> [ testCase "accepts a schema using everything the contract allows" $
               allViolations goodSchema @?= []
           ]
    )

-- * Canned replies

{- | A complete streamed (SSE) reply: an HTTP 200 whose body is these events,
each as an @event:@ line and a @data:@ line, as the Messages streaming
documentation shapes them. The adapter asks for a stream, so a plain JSON
reply would not be read.
-}
sseReply :: [(ByteString, Value)] -> ByteString
sseReply events =
  "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nConnection: close\r\n\r\n"
    <> mconcat ["event: " <> name <> "\ndata: " <> BL.toStrict (encode payload) <> "\n\n" | (name, payload) <- events]

-- | The opening @message_start@ event: an empty assistant message, no stop reason yet.
messageStart :: (ByteString, Value)
messageStart =
  ( "message_start"
  , object
      [ "type" .= ("message_start" :: Text)
      , "message"
          .= object
            [ "id" .= ("msg_wire_contract" :: Text)
            , "type" .= ("message" :: Text)
            , "role" .= ("assistant" :: Text)
            , "model" .= ("wire-contract-model" :: Text)
            , "content" .= ([] :: [Value])
            , "stop_reason" .= Null
            , "stop_sequence" .= Null
            , "usage" .= object ["input_tokens" .= (10 :: Int), "output_tokens" .= (1 :: Int)]
            ]
      ]
  )

-- | The closing @message_delta@ event, which carries the stop reason, with any further members of its @delta@.
messageDelta :: Text -> [(Key.Key, Value)] -> (ByteString, Value)
messageDelta stopReason extra =
  ( "message_delta"
  , object
      [ "type" .= ("message_delta" :: Text)
      , "delta" .= object (["stop_reason" .= stopReason, "stop_sequence" .= Null] <> extra)
      , "usage" .= object ["output_tokens" .= (5 :: Int)]
      ]
  )

-- | The final @message_stop@ event.
messageStop :: (ByteString, Value)
messageStop = ("message_stop", object ["type" .= ("message_stop" :: Text)])

{- | A streamed reply of one text block, ending @stop_reason: end_turn@.

Source: [S8], the streaming event flow. Read 2026-10-12.
-}
anthropicTextReply :: Text -> ByteString
anthropicTextReply answer =
  sseReply
    [ messageStart
    , ("content_block_start", object ["type" .= ("content_block_start" :: Text), "index" .= (0 :: Int), "content_block" .= object ["type" .= ("text" :: Text), "text" .= ("" :: Text)]])
    , ("content_block_delta", object ["type" .= ("content_block_delta" :: Text), "index" .= (0 :: Int), "delta" .= object ["type" .= ("text_delta" :: Text), "text" .= answer]])
    , ("content_block_stop", object ["type" .= ("content_block_stop" :: Text), "index" .= (0 :: Int)])
    , messageDelta "end_turn" []
    , messageStop
    ]

{- | A streamed reply of one @tool_use@ block, ending @stop_reason: tool_use@:
the block starts with an empty @input@ and the input arrives as
@input_json_delta@ chunks of @partial_json@, an empty one first and the JSON
cut in two after it, as in [S8]'s tool-use example.
-}
anthropicToolUseReply :: Text -> Text -> Value -> ByteString
anthropicToolUseReply toolId toolName input =
  sseReply $
    [ messageStart
    ,
      ( "content_block_start"
      , object
          [ "type" .= ("content_block_start" :: Text)
          , "index" .= (0 :: Int)
          , "content_block" .= object ["type" .= ("tool_use" :: Text), "id" .= toolId, "name" .= toolName, "input" .= object []]
          ]
      )
    ]
      <> fmap inputDelta ["", firstHalf, secondHalf]
      <> [ ("content_block_stop", object ["type" .= ("content_block_stop" :: Text), "index" .= (0 :: Int)])
         , messageDelta "tool_use" []
         , messageStop
         ]
  where
    (firstHalf, secondHalf) = T.splitAt (T.length json `div` 2) json
    json = showValue input
    inputDelta :: Text -> (ByteString, Value)
    inputDelta chunk =
      ( "content_block_delta"
      , object ["type" .= ("content_block_delta" :: Text), "index" .= (0 :: Int), "delta" .= object ["type" .= ("input_json_delta" :: Text), "partial_json" .= chunk]]
      )

{- | A streamed reply that declines: no content, @stop_reason: "refusal"@ in
the @message_delta@, and a @stop_details@ naming a category, in the shape
[S7]'s "What a refusal looks like" gives for the message. That it rides in
the @message_delta@ of a stream is @baikai-claude@'s reading, see
'refusalIsRefusal'.
-}
anthropicRefusalReply :: ByteString
anthropicRefusalReply =
  sseReply
    [ messageStart
    , messageDelta
        "refusal"
        [ "stop_details"
            .= object
              [ "type" .= ("refusal" :: Text)
              , "category" .= ("cyber" :: Text)
              , "explanation" .= ("This request was declined because it could enable cyber harm." :: Text)
              ]
        ]
    , messageStop
    ]
