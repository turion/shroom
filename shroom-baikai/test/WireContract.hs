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
(The @tool_use_id@ and the refusal branch are the /response/ leg, which a
later part of this suite will own.)

= How to read a failure

__A failure is fixed in the emitter, not in the expectation.__ Every assertion
below is a citation of Anthropic's documentation, and the citation carries the
date it was read. Changing an expectation means overruling a citation: re-read
the source, and update the citation (text and date) in the same revision.
Never loosen an assertion to make it pass.

= The accepted schema keywords

The schema keywords Anthropic's structured outputs accept — the allowlist
'allowedKeywords' that 'unsupportedKeywords' enforces. A keyword outside this
list may not appear anywhere in the emitted schema, whatever a lenient host
does with it (this is an allowlist, not a list of known offenders):

* @type@ (@object@, @array@, @string@, @integer@, @number@, @boolean@, @null@),
  @enum@, @const@, @default@
* @properties@, @required@, @additionalProperties@ (only ever @false@)
* @items@, @minItems@ (only 0 or 1)
* @anyOf@, @allOf@
* @$ref@, @$defs@, @definitions@ (never an external @$ref@; never recursive)
* @format@ (only on a string, only one of @date-time@, @time@, @date@,
  @duration@, @email@, @hostname@, @uri@, @ipv4@, @ipv6@, @uuid@)
* @pattern@
* @description@, @title@

Authority, all read on 2026-10-12:

[S1] Anthropic, "Structured outputs", section "JSON Schema limitations"
(@platform.claude.com\/docs\/en\/build-with-claude\/structured-outputs@),
its \"Supported features\" and \"Not supported\" lists, and its \"Pattern
support (regex)\" list. The page spells @$defs@ as @$def@ in the list; this
suite reads that as the keyword @$defs@ (and accepts @definitions@, which the
same line names).

[S2] The same page's @output_config.format@ request-shape example: @type@ and
@schema@ as siblings.

[S3] Anthropic, "API overview" (@platform.claude.com\/docs\/en\/api\/overview@),
\"Available APIs\" (@POST \/v1\/messages@) and \"Authentication\" (the
@anthropic-version@, @content-type@ and credential headers).

[S4] @anthropic-sdk-python@, @src\/anthropic\/lib\/_parse\/_transform.py@,
@transform_schema@: Anthropic's own helper copies @title@ and @description@
through, sends @format@ only on a string and only from the supported list,
and appends everything it does not send (a number's @format@, @maxItems@,
@prefixItems@, ...) to the @description@ instead. The documentation page
says nothing about @title@; this is the only authority for accepting it.

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

The emitter currently violates the accepted set for two shapes, so the
test type 'Gadget' deliberately contains neither, and the suite is green on a
state that is not yet correct:

* a number's @format@: every 'Double' goes out as @{\"type\":\"number\",
  \"format\":\"double\"}@, and [S1]\/[S4] list @format@ for strings only;
* a constructor with two or more fields, which goes out as a tuple: @items@
  as an array, @maxItems: 2@ and @minItems: 2@, against [S1]'s "Array
  constraints beyond @minItems@ of 0 or 1" and [S4]'s dropping of @maxItems@.

'checkerRejects' proves the checks do flag both shapes, so the day 'Gadget'
gains a 'Double' the failure will be real.
-}
module Main (main) where

-- base
import Control.Concurrent (forkIO, killThread)
import Control.Exception (SomeException, bracket, handle)
import Control.Monad (foldM, forM_, forever, void)
import Data.Char (isAsciiUpper)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Maybe (fromMaybe, isJust)
import GHC.Generics (Generic)
import System.Timeout (timeout)

-- bytestring
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL

-- text
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)

-- aeson
import Data.Aeson (FromJSON, ToJSON, Value (..), decodeStrict, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap

-- vector
import Data.Vector qualified as V

-- network
import Network.Socket (
  Family (AF_INET),
  PortNumber,
  SockAddr (SockAddrInet),
  Socket,
  SocketType (Stream),
  accept,
  bind,
  close,
  defaultProtocol,
  getSocketName,
  listen,
  socket,
  tupleToHostAddress,
 )
import Network.Socket.ByteString (recv, sendAll)

-- openapi3
import Data.OpenApi (ToSchema)

-- generics-sop
import Generics.SOP qualified as SOP

-- tasty
import Test.Tasty (TestTree, defaultMain, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- shroom
import Control.Monad.Prompt (Promptable)
import Control.Monad.Prompt.Baikai (anthropicCompatBackend)
import Control.Monad.Prompt.Effect (PromptConfig (..), defaultPromptConfig)
import Control.Monad.Prompt.Effect qualified as Effect
import Data.Shroom.Class (Describable (..), Surveyable)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "shroom-baikai wire contract (request)"
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

-- * The exchange

-- | One recorded request.
data Captured = Captured
  { requestLine :: ByteString
  , headers :: [(ByteString, ByteString)]
  -- ^ Header names lower-cased.
  , body :: ByteString
  }

-- | What came of one run of the library against the listener.
data Exchange = Exchange
  { result :: Either Text Gadget
  , requests :: [Captured]
  }

{- | Prompt for a 'Gadget' through the library's own front door
('Effect.prompt', not a hand-built request), against a listener that answers
once with 'cannedGadget'. Runs once; every assertion reads the same capture.
-}
capture :: IO Exchange
capture =
  withCannedServer [anthropicTextReply (encodeText cannedGadget)] $ \port recorded -> do
    backend <- anthropicCompatBackend ("http://127.0.0.1:" <> T.pack (show port)) "wire-contract-model" "wire-contract-key"
    outcome <-
      timeout 10_000_000 $
        Effect.runPromptResultEff backend defaultPromptConfig {maxRetries = 0} (Effect.prompt @Gadget)
    recorded' <- recorded
    case outcome of
      Nothing -> assertFailure "the call did not finish within 10s"
      Just r -> pure (Exchange r recorded')
  where
    encodeText = decodeUtf8 . BL.toStrict . encode

-- | The single request of the exchange, or a failure saying how many there were.
theRequest :: IO Exchange -> IO Captured
theRequest getExchange = do
  ex <- getExchange
  case ex.requests of
    [r] -> pure r
    rs -> assertFailure ("expected exactly one request, got " <> show (length rs))

-- | The request body, decoded to a JSON object.
requestBody :: IO Exchange -> IO Value
requestBody getExchange = do
  r <- theRequest getExchange
  case decodeStrict r.body of
    Just v@(Object _) -> pure v
    other -> assertFailure ("the request body is not a JSON object: " <> show other <> " from " <> show r.body)

-- | The @output_config.format@ object of the request, or a failure saying it is absent.
outputConfigFormat :: IO Exchange -> IO (KeyMap.KeyMap Value)
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
emittedSchema :: IO Exchange -> IO Value
emittedSchema getExchange = do
  fmt <- outputConfigFormat getExchange
  case KeyMap.lookup "schema" fmt of
    Just s@(Object _) -> pure s
    other -> assertFailure ("output_config.format.schema is not a JSON object: " <> show other)

{- | The call completed: one request was made, and the canned answer was
parsed back into the 'Gadget' it said. A guard on the harness: were it to
fail, every other assertion would be reading the wrong thing.
-}
exchangeCompletes :: IO Exchange -> IO ()
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
requestLineAndHeaders :: IO Exchange -> IO ()
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
envelopePresent :: IO Exchange -> IO ()
envelopePresent getExchange = void (outputConfigFormat getExchange)

{- | The envelope has the shape observed on the wire and shown in the module
header, which is [S2]'s: @type: "json_schema"@ and @schema@ as the only
members of @output_config.format@ — no @name@, no @strict@ — and no OpenAI
style top-level @response_format@.

Source: [S2]. Read 2026-10-12. The observed shape is not an extra
requirement over [S2]; it is what was recorded, and a change to it is a
change to review against [S2].
-}
envelopeIsFlat :: IO Exchange -> IO ()
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
schemaCarriesTheType :: IO Exchange -> IO ()
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
schemaUsesAllowedKeywords :: IO Exchange -> IO ()
schemaUsesAllowedKeywords getExchange = emittedSchema getExchange >>= expectNone . unsupportedKeywords

{- | @additionalProperties: false@ on every object — a node that is of
@type: object@ or has @properties@ — and never anything but @false@.

Source: [S1] "Supported features" (@additionalProperties@ "must be set to
@false@ for objects") and "Not supported". Read 2026-10-12.
-}
everyObjectIsClosed :: IO Exchange -> IO ()
everyObjectIsClosed getExchange = emittedSchema getExchange >>= expectNone . objectsNotClosed

{- | No @minimum@, @maximum@ or @multipleOf@ survives anywhere (the emitter
folds a bound into the field's @description@ instead, which [S1]'s SDK
paragraph describes).

Source: [S1] "Not supported" (numerical constraints, such as @minimum@,
@maximum@, @multipleOf@). Read 2026-10-12.
-}
noNumericBounds :: IO Exchange -> IO ()
noNumericBounds getExchange = emittedSchema getExchange >>= expectNone . numericBounds

{- | Every @$ref@ is internal (starts @#\/@) and resolves to a node in the
schema. Today's emitter inlines everything, so there are none; the assertion
is on the contract, so that a rewrite that does emit @$defs@ is held to it.

Source: [S1] "Supported features" (@$ref@, @$defs@ and @definitions@;
"external @$ref@ not supported"). Read 2026-10-12.
-}
noExternalRefs :: IO Exchange -> IO ()
noExternalRefs getExchange = emittedSchema getExchange >>= expectNone . badRefs

{- | No recursion: no @$ref@ leads, through other @$ref@s, back to itself.
Today's emitter caps its own inlining at 20 levels, which for a recursive
type would leave a truncated schema rather than a cycle; this guards the day
it emits @$defs@ instead.

Source: [S1] "Not supported" (recursive schemas). Read 2026-10-12.
-}
noRecursion :: IO Exchange -> IO ()
noRecursion getExchange = emittedSchema getExchange >>= expectNone . recursion

{- | The remaining documented per-keyword constraints: @format@ only on a
string and only from the supported list; @minItems@ only 0 or 1; @enum@
holding no arrays or objects; @items@ one schema, not a tuple; no @$ref@
under @allOf@.

Source: [S1] "Supported features" (the @enum@, @allOf@, string-format and
@minItems@ lines) and "Not supported" (array constraints); [S4] for a
number's @format@. Read 2026-10-12.
-}
otherConstraintsHold :: IO Exchange -> IO ()
otherConstraintsHold getExchange = emittedSchema getExchange >>= expectNone . otherConstraints

expectNone :: [Text] -> IO ()
expectNone [] = pure ()
expectNone vs = assertFailure (T.unpack (T.unlines ("the emitted schema violates Anthropic's documented subset:" : vs)))

-- * The checks

{- | The keywords Anthropic accepts; see the module header for each one's
authority.
-}
allowedKeywords :: [Text]
allowedKeywords =
  [ "type"
  , "enum"
  , "const"
  , "default"
  , "properties"
  , "required"
  , "additionalProperties"
  , "items"
  , "minItems"
  , "anyOf"
  , "allOf"
  , "$ref"
  , "$defs"
  , "definitions"
  , "format"
  , "pattern"
  , "description"
  , "title"
  ]

-- | The string formats Anthropic enforces ([S1], [S4]).
stringFormats :: [Text]
stringFormats = ["date-time", "time", "date", "duration", "email", "hostname", "uri", "ipv4", "ipv6", "uuid"]

{- | Every schema node of a schema, with a readable path. A node is reached as a
value of @properties@, @$defs@ or @definitions@, as @items@, as an element of
@anyOf@ or @allOf@, or as an object-valued @additionalProperties@. Keys in
those positions that name a property or a definition are names, not keywords,
and values of @enum@, @const@ and @default@ are data: neither is a node.
-}
schemaNodes :: Value -> [(Text, KeyMap.KeyMap Value)]
schemaNodes = go "#"
  where
    go path (Object o) = (path, o) : concatMap (child path) (KeyMap.toList o)
    go _ _ = []
    child path (k, v)
      | k `elem` namedChildren, Object named <- v = concat [go (path <> "/" <> Key.toText k <> "/" <> Key.toText n) s | (n, s) <- KeyMap.toList named]
      | k `elem` listedChildren, Array vs <- v = concat [go (path <> "/" <> Key.toText k <> "[" <> T.pack (show i) <> "]") s | (i, s) <- zip [0 :: Int ..] (V.toList vs)]
      | k `elem` singleChildren = go (path <> "/" <> Key.toText k) v
      | otherwise = []
    namedChildren, listedChildren, singleChildren :: [Key.Key]
    namedChildren = ["properties", "$defs", "definitions"]
    listedChildren = ["anyOf", "allOf", "oneOf"]
    singleChildren = ["items", "additionalProperties"]

hasType :: Text -> KeyMap.KeyMap Value -> Bool
hasType t o = case KeyMap.lookup "type" o of
  Just (String t') -> t == t'
  Just (Array ts) -> String t `elem` V.toList ts
  _ -> False

-- | A keyword outside 'allowedKeywords'.
unsupportedKeywords :: Value -> [Text]
unsupportedKeywords root =
  [ path <> ": keyword " <> Key.toText k <> " is outside Anthropic's accepted set"
  | (path, o) <- schemaNodes root
  , k <- KeyMap.keys o
  , Key.toText k `notElem` allowedKeywords
  ]

-- | An object (of @type: object@, or with @properties@) not closed, or an @additionalProperties@ that is not @false@.
objectsNotClosed :: Value -> [Text]
objectsNotClosed root =
  concat
    [ [path <> ": object without additionalProperties: false" | isObject, KeyMap.lookup "additionalProperties" o /= Just (Bool False)]
        <> [path <> ": additionalProperties is " <> showValue ap <> ", must be false" | Just ap <- [KeyMap.lookup "additionalProperties" o], ap /= Bool False]
    | (path, o) <- schemaNodes root
    , let isObject = hasType "object" o || KeyMap.member "properties" o
    ]

-- | @minimum@, @maximum@ or @multipleOf@ anywhere.
numericBounds :: Value -> [Text]
numericBounds root =
  [ path <> ": numeric constraint " <> Key.toText k <> " survives"
  | (path, o) <- schemaNodes root
  , k <- ["minimum", "maximum", "multipleOf", "exclusiveMinimum", "exclusiveMaximum"]
  , KeyMap.member k o
  ]

{- | A @$ref@ that is not internal or does not resolve, or sits in an @allOf@ (which
[S1] does not support).
-}
badRefs :: Value -> [Text]
badRefs root =
  concat
    [ [path <> ": $ref " <> showValue ref <> " is not an internal reference (#/...)" | notInternal ref]
        <> [path <> ": $ref " <> showValue ref <> " does not resolve" | not (notInternal ref), Nothing <- [resolveRef root ref]]
    | (path, o) <- schemaNodes root
    , Just ref <- [KeyMap.lookup "$ref" o]
    ]
  where
    notInternal (String r) = not ("#/" `T.isPrefixOf` r)
    notInternal _ = True

{- | A cycle through @$ref@s: following references from any node reaches a
reference already being followed.
-}
recursion :: Value -> [Text]
recursion root = concat [visit [] ref | (_, o) <- schemaNodes root, Just (String ref) <- [KeyMap.lookup "$ref" o]]
  where
    visit stack ref
      | ref `elem` stack = ["recursive: " <> T.intercalate " -> " (reverse (ref : stack))]
      | otherwise = case resolveRef root (String ref) of
          Nothing -> []
          Just target -> concat [visit (ref : stack) r | (_, o) <- schemaNodes target, Just (String r) <- [KeyMap.lookup "$ref" o]]

-- | @format@, @minItems@, @enum@, @items@ and @allOf@ outside what [S1] allows.
otherConstraints :: Value -> [Text]
otherConstraints root =
  concat
    [ mconcat
        [ [ path <> ": format " <> showValue f <> " is not a supported string format on a string"
          | Just f <- [KeyMap.lookup "format" o]
          , not (hasType "string" o && isSupportedFormat f)
          ]
        , [ path <> ": minItems " <> showValue n <> ", only 0 and 1 are supported"
          | Just n <- [KeyMap.lookup "minItems" o]
          , n `notElem` ([Number 0, Number 1] :: [Value])
          ]
        , [ path <> ": enum with a complex value"
          | Just (Array vs) <- [KeyMap.lookup "enum" o]
          , any isComplex (V.toList vs)
          ]
        , [path <> ": items is a tuple (an array), not one schema" | Just (Array _) <- [KeyMap.lookup "items" o]]
        , [ path <> ": allOf with a $ref"
          | Just (Array alts) <- [KeyMap.lookup "allOf" o]
          , any (\case Object ao -> KeyMap.member "$ref" ao; _ -> False) (V.toList alts)
          ]
        ]
    | (path, o) <- schemaNodes root
    ]
  where
    isSupportedFormat (String f) = f `elem` stringFormats
    isSupportedFormat _ = False
    isComplex (Array _) = True
    isComplex (Object _) = True
    isComplex _ = False

-- | Every check, for the self-test.
allViolations :: Value -> [Text]
allViolations v = concatMap ($ v) checks
  where
    checks :: [Value -> [Text]]
    checks = [unsupportedKeywords, objectsNotClosed, numericBounds, badRefs, recursion, otherConstraints]

showValue :: Value -> Text
showValue = decodeUtf8 . BL.toStrict . encode

-- | Follow an internal @$ref@ such as @#\/$defs\/X@ from the root.
resolveRef :: Value -> Value -> Maybe Value
resolveRef root (String r) | Just path <- T.stripPrefix "#/" r = foldl (\acc seg -> acc >>= member seg) (Just root) (T.splitOn "/" path)
resolveRef _ _ = Nothing

member :: Text -> Value -> Maybe Value
member k (Object o) = KeyMap.lookup (Key.fromText k) o
member _ _ = Nothing

{- | Navigate from the root by property name, @[]@ meaning "the @items@ of an
array", following a @$ref@ at each step. 'Nothing' if a step is missing.
-}
nodeAt :: Value -> [Text] -> Maybe Value
nodeAt root path = deref root root >>= \start -> foldM (\v step -> next step v >>= deref root) start path
  where
    next "[]" v = member "items" v
    next name v = member "properties" v >>= member name
    deref r v = case member "$ref" v of
      Just ref -> resolveRef r ref >>= deref r
      Nothing -> Just v

propertyNames :: Value -> [Text]
propertyNames (Object o) | Just (Object ps) <- KeyMap.lookup "properties" o = sort (Key.toText <$> KeyMap.keys ps)
propertyNames _ = []

requiredNames :: Value -> [Text]
requiredNames (Object o) | Just (Array rs) <- KeyMap.lookup "required" o = sort [r | String r <- V.toList rs]
requiredNames _ = []

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

badSchemas :: [(Text, Text, Value)]
badSchemas =
  [ ("an object without additionalProperties", "without additionalProperties", object ["type" .= str "object", "properties" .= object []])
  , ("additionalProperties: true", "must be false", object ["type" .= str "object", "properties" .= object [], "additionalProperties" .= True])
  , ("a nested object left open", "without additionalProperties", closedNode ["inner" .= object ["type" .= str "object", "properties" .= object []]])
  , ("a minimum", "minimum", closedNode ["n" .= object ["type" .= str "integer", "minimum" .= (1 :: Int)]])
  , ("a maximum", "maximum", closedNode ["n" .= object ["type" .= str "integer", "maximum" .= (9 :: Int)]])
  , ("oneOf", "keyword oneOf", closedNode ["v" .= object ["oneOf" .= ([] :: [Value])]])
  , ("an unknown keyword anywhere", "keyword nullable", closedNode ["v" .= object ["type" .= str "string", "nullable" .= True]])
  , ("an external $ref", "not an internal reference", closedNode ["v" .= object ["$ref" .= str "http://example.com/x.json"]])
  , ("a $ref that does not resolve", "does not resolve", closedNode ["v" .= object ["$ref" .= str "#/$defs/Missing"]])
  , ("recursion through $defs", "recursive", withDefs ["Tree" .= closedNode ["child" .= object ["$ref" .= str "#/$defs/Tree"]]] ["root" .= object ["$ref" .= str "#/$defs/Tree"]])
  , ("a number's format", "format \"double\"", closedNode ["d" .= object ["type" .= str "number", "format" .= str "double"]])
  , ("an unsupported string format", "format \"int64\"", closedNode ["d" .= object ["type" .= str "string", "format" .= str "int64"]])
  , ("a tuple", "tuple", closedNode ["t" .= object ["type" .= str "array", "items" .= ([object ["type" .= str "string"]] :: [Value])]])
  , ("a maxItems", "keyword maxItems", closedNode ["t" .= object ["type" .= str "array", "items" .= object ["type" .= str "string"], "maxItems" .= (2 :: Int)]])
  , ("minItems above 1", "only 0 and 1", closedNode ["t" .= object ["type" .= str "array", "items" .= object ["type" .= str "string"], "minItems" .= (2 :: Int)]])
  , ("allOf with a $ref", "allOf with a $ref", withDefs ["X" .= closedNode []] ["v" .= object ["allOf" .= ([object ["$ref" .= str "#/$defs/X"]] :: [Value])]])
  ]
  where
    str :: Text -> Text
    str = id

goodSchema :: Value
goodSchema =
  withDefs
    ["Leaf" .= closedNode ["s" .= object ["type" .= ("string" :: Text), "format" .= ("email" :: Text), "title" .= ("S" :: Text), "description" .= ("d" :: Text)]]]
    [ "leaf" .= object ["$ref" .= ("#/$defs/Leaf" :: Text)]
    , "list" .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("integer" :: Text)], "minItems" .= (1 :: Int)]
    , "pick" .= object ["anyOf" .= ([object ["type" .= ("string" :: Text), "enum" .= (["a", "b"] :: [Text])], object ["type" .= ("null" :: Text)]] :: [Value])]
    , "p" .= object ["type" .= ("string" :: Text), "pattern" .= ("^a+$" :: Text), "default" .= ("a" :: Text)]
    ]

-- | A closed object node with the given properties.
closedNode :: [(Key.Key, Value)] -> Value
closedNode props = object ["type" .= ("object" :: Text), "properties" .= object props, "additionalProperties" .= False]

-- | A closed root object with the given properties and @$defs@.
withDefs :: [(Key.Key, Value)] -> [(Key.Key, Value)] -> Value
withDefs defs props = case closedNode props of
  Object o -> Object (KeyMap.insert "$defs" (object defs) o)
  v -> v

-- * The listener

{- | Run an action against a loopback HTTP listener. It binds port 0 on
@127.0.0.1@ (the OS picks a free port, so runs cannot collide), serves one
canned reply per request in order, answers 500 once they run out, and records
every request. The action gets the port and a read of what has been recorded
so far; the listener records a request before it replies, so a read after the
client call returned sees that request.

Each reply is a complete HTTP response and closes its connection, so every
request arrives on a fresh connection.
-}
withCannedServer :: [ByteString] -> (PortNumber -> IO [Captured] -> IO a) -> IO a
withCannedServer replies act =
  bracket (socket AF_INET Stream defaultProtocol) close $ \listener -> do
    bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
    listen listener 4
    port <-
      getSocketName listener >>= \case
        SockAddrInet p _ -> pure p
        other -> fail ("unexpected listener address: " <> show other)
    recorded <- newIORef []
    remaining <- newIORef replies
    let serve = forever $ do
          (conn, _) <- accept listener
          handle (\(_ :: SomeException) -> pure ()) . bracket (pure conn) close $ \c -> do
            req <- readRequest c
            modifyIORef' recorded (<> [req])
            next <- atomicModifyIORef' remaining (\case r : rest -> (rest, Just r); [] -> ([], Nothing))
            sendAll c (fromMaybe internalServerError next)
    bracket (forkIO serve) killThread (\_ -> act port (readIORef recorded))
  where
    internalServerError :: ByteString
    internalServerError = "HTTP/1.1 500 Internal Server Error\r\nConnection: close\r\nContent-Length: 0\r\n\r\n"

{- | Read one HTTP request off the socket: request line, headers (names
lower-cased) and a body as framed by @Content-Length@ (a request without one
is a failure rather than a mis-parse).
-}
readRequest :: Socket -> IO Captured
readRequest conn = go BS.empty
  where
    go acc = case BS.breakSubstring "\r\n\r\n" acc of
      (headBlock, rest)
        | not (BS.null rest) -> do
            let (reqLine, hdrs) = case BC.lines (BC.filter (/= '\r') headBlock) of
                  l : ls -> (l, [(BC.map lower k, BC.strip (BS.drop 1 v)) | line <- ls, let (k, v) = BC.break (== ':') line])
                  [] -> (BS.empty, [])
            case lookup "content-length" hdrs >>= fmap fst . BC.readInt of
              Nothing -> fail ("request without a Content-Length: " <> show headBlock)
              Just n -> do
                bodyBytes <- readUpTo n (BS.drop 4 rest)
                pure Captured {requestLine = reqLine, headers = hdrs, body = bodyBytes}
      _ -> more acc >>= maybe (fail ("connection closed mid-request: " <> show acc)) go
    readUpTo n acc
      | BS.length acc >= n = pure (BS.take n acc)
      | otherwise = more acc >>= maybe (fail "connection closed mid-body") (readUpTo n)
    more acc = recv conn 4096 >>= \chunk -> pure (if BS.null chunk then Nothing else Just (acc <> chunk))
    lower c = if isAsciiUpper c then toEnum (fromEnum c + 32) else c

{- | A complete streamed (SSE) Anthropic Messages reply, as the Messages
streaming documentation shapes it: @message_start@, one text block, a
@message_delta@ carrying @stop_reason: end_turn@, @message_stop@. The adapter
asks for a stream, so a plain JSON reply would not be read.
-}
anthropicTextReply :: Text -> ByteString
anthropicTextReply answer =
  "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nConnection: close\r\n\r\n"
    <> mconcat
      [ event
          "message_start"
          ( object
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
      , event
          "content_block_start"
          (object ["type" .= ("content_block_start" :: Text), "index" .= (0 :: Int), "content_block" .= object ["type" .= ("text" :: Text), "text" .= ("" :: Text)]])
      , event
          "content_block_delta"
          (object ["type" .= ("content_block_delta" :: Text), "index" .= (0 :: Int), "delta" .= object ["type" .= ("text_delta" :: Text), "text" .= answer]])
      , event "content_block_stop" (object ["type" .= ("content_block_stop" :: Text), "index" .= (0 :: Int)])
      , event
          "message_delta"
          ( object
              [ "type" .= ("message_delta" :: Text)
              , "delta" .= object ["stop_reason" .= ("end_turn" :: Text), "stop_sequence" .= Null]
              , "usage" .= object ["output_tokens" .= (5 :: Int)]
              ]
          )
      , event "message_stop" (object ["type" .= ("message_stop" :: Text)])
      ]
  where
    event :: ByteString -> Value -> ByteString
    event name payload = "event: " <> name <> "\ndata: " <> BL.toStrict (encode payload) <> "\n\n"
