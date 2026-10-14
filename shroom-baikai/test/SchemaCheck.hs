{- | A pure checker for a JSON Schema against the subset Anthropic's structured
outputs accept, and the fixtures that prove it can fail. No I/O: the wire
contract suite ("WireContract") feeds it the schema captured off the wire.

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

= Known gaps

The emitter violates this subset in more shapes than the wire suite's test
type exercises; see "Known gaps the test type avoids" in "WireContract", which
lists them and where they are tracked.
-}
module SchemaCheck (
  -- * The checks
  allowedKeywords,
  stringFormats,
  schemaNodes,
  unsupportedKeywords,
  objectsNotClosed,
  numericBounds,
  badRefs,
  recursion,
  otherConstraints,
  allViolations,

  -- * Reading a schema
  showValue,
  resolveRef,
  nodeAt,
  propertyNames,
  requiredNames,

  -- * Fixtures for checking the checks
  badSchemas,
  goodSchema,
) where

-- base
import Control.Monad (foldM)
import Data.List (sort)

-- bytestring
import Data.ByteString.Lazy qualified as BL

-- text
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)

-- aeson
import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap

-- vector
import Data.Vector qualified as V

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
