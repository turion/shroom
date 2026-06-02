{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Built-in web tools: 'WebFetch', 'DuckDuckGoSearch', and 'WikipediaSearch'.

Use these as ready-made 'ToolHandler' values in your 'runPromptT' call:

@
result <- runPromptResultTWith cfg $
  runPromptT defaultPromptConfig
    (webFetchHandler :* duckDuckGoSearchHandler :* wikipediaSearchHandler :* Nil)
    myProgram
@

* 'duckDuckGoSearchHandler' — DuckDuckGo Instant Answer API. Best for named
  entities (people, places, concepts) that have a Wikipedia article.
  Returns a short abstract and a URL to fetch for more detail.
* 'wikipediaSearchHandler' — Wikipedia OpenSearch. Returns up to 5
  article titles and URLs for any query. Use this when the DDG search
  returns no result and you need to find Wikipedia pages to fetch.
* 'webFetchHandler' — HTTP GET, strips HTML, truncates to 2000 chars.
-}
module Control.Monad.Prompt.Tool.Web (module Control.Monad.Prompt.Tool.Web) where

-- base
import Control.Exception (SomeException, try)
import Data.Char (isAlphaNum)

-- text
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

-- aeson
import Data.Aeson (FromJSON, ToJSON, withObject, (.:?))
import Data.Aeson qualified as Aeson
import GHC.Generics (Generic)

-- vector
import Data.Vector qualified as V

-- containers
import Data.Set qualified as S

-- openapi3
import Data.OpenApi (ToSchema)

-- tagsoup
import Text.HTML.TagSoup (Tag, innerText, parseTags)

-- modern-uri
import Text.URI (mkURI)

-- req
import Network.HTTP.Req (
  GET (..),
  NoReqBody (..),
  bsResponse,
  defaultHttpConfig,
  https,
  req,
  responseBody,
  runReq,
  useHttpURI,
  useHttpsURI,
  (/:),
 )
import Network.HTTP.Req qualified as Req

-- universe-base
import Data.Universe.Class (Universe)

-- shroom

import Control.Monad.Prompt.TH (deriveDescribeType)
import Control.Monad.Prompt.Tool (IsTool (..), ToolHandler (..))
import Data.Describe (Describe (..))

-- * WebFetch

{- | A request to fetch a URL via HTTP GET.
The LLM provides the URL; the handler returns the page text.
-}
newtype WebFetch = WebFetch
  { fetchUrl :: Text
  -- ^ The URL to fetch.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- We need a new declaration group so TH can reify the type defined above.
$(pure [])

data WebFetchProperty
  = -- | Must start with http:// or https://
    WebFetchUrlScheme
  | -- | Must contain only URL-safe characters
    WebFetchUrlSafeChars
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

instance Describe WebFetch where
  type Property WebFetch = WebFetchProperty
  describeType = $(deriveDescribeType ''WebFetch)
  describeProperties _ WebFetchUrlScheme =
    Just "The URL must start with \"https://\" or \"http://\"."
  describeProperties _ WebFetchUrlSafeChars =
    Just "The URL must contain only URL-safe characters: alphanumerics and \"-._~:/?#[]@!$&'()*+,;=%\". No spaces, no angle brackets, no backslash, no shell special characters."
  propertyHolds (WebFetch url) WebFetchUrlScheme =
    "https://" `T.isPrefixOf` url || "http://" `T.isPrefixOf` url
  propertyHolds (WebFetch url) WebFetchUrlSafeChars =
    T.all (\c -> isAlphaNum c || c `elem` ("-._~:/?#[]@!$&'()*+,;=%" :: String)) url
  examples _ =
    S.fromList
      [ WebFetch "https://example.com"
      , WebFetch "https://en.wikipedia.org/wiki/Haskell_%28programming_language%29"
      ]

instance IsTool WebFetch where
  toolDescription _ = Just "Returns up to 2000 characters of the page body."

-- | Perform an HTTP GET, strip HTML tags, truncate to 2000 characters.
userAgentHeader :: Req.Option scheme
userAgentHeader = Req.header "User-Agent" "shroom/0.1 (https://github.com/turion/shroom)"

webFetchHandler :: ToolHandler WebFetch
webFetchHandler = ToolHandler $ \(WebFetch url) -> do
  result <- try @SomeException $ runReq defaultHttpConfig $ do
    uri <- mkURI url
    case useHttpsURI uri of
      Just (u, opts) -> do
        r <- req GET u NoReqBody bsResponse (opts <> userAgentHeader)
        pure $ Right (responseBody r)
      Nothing ->
        case useHttpURI uri of
          Just (u, opts) -> do
            r <- req GET u NoReqBody bsResponse (opts <> userAgentHeader)
            pure $ Right (responseBody r)
          Nothing -> pure $ Left ("WebFetch: could not parse URL: " <> url)
  case result of
    Left ex -> pure $ Left ("WebFetch error: " <> T.pack (show ex))
    Right (Left e) -> pure $ Left e
    Right (Right bs) ->
      let txt = TE.decodeUtf8Lenient bs
          -- Strip HTML tags using tagsoup
          stripped = innerText (parseTags txt :: [Tag Text])
          -- Collapse whitespace
          cleaned = T.unwords . filter (not . T.null) . T.words $ stripped
       in pure $ Right (T.take 2000 cleaned)

-- * DuckDuckGoSearch

{- | A DuckDuckGo Instant Answer search query.

__Limitations__: This uses the DuckDuckGo Instant Answer API, which only returns
results for well-known named entities (people, places, concepts) that have a
dedicated Wikipedia article. Vague or multi-word queries (e.g. @\"celebrity names\"@,
@\"Haskell conference speakers\"@) will return @\"No result found\"@.
For general search use 'WikipediaSearch' instead.
-}
newtype DuckDuckGoSearch = DuckDuckGoSearch
  { searchQuery :: Text
  -- ^ The search query string.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- We need a new declaration group so TH can reify the type defined above.
$(pure [])

data DuckDuckGoSearchProperty
  = -- | Must not be empty
    DuckDuckGoSearchQueryNotEmpty
  | -- | Only alphanumerics, spaces, basic punctuation
    DuckDuckGoSearchQuerySafeChars
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

instance Describe DuckDuckGoSearch where
  type Property DuckDuckGoSearch = DuckDuckGoSearchProperty
  describeType = $(deriveDescribeType ''DuckDuckGoSearch)
  describeProperties _ DuckDuckGoSearchQueryNotEmpty =
    Just "The search query must not be empty."
  describeProperties _ DuckDuckGoSearchQuerySafeChars =
    Just "The search query must contain only alphanumerics, spaces, and basic punctuation (\".,'-\"). No newlines, no HTML, no shell special characters."
  propertyHolds (DuckDuckGoSearch q) DuckDuckGoSearchQueryNotEmpty = not (T.null q)
  propertyHolds (DuckDuckGoSearch q) DuckDuckGoSearchQuerySafeChars =
    T.all (\c -> isAlphaNum c || c `elem` (" .,'-" :: String)) q
  examples _ =
    S.fromList
      [ DuckDuckGoSearch "Haskell programming language"
      , DuckDuckGoSearch "Simon Peyton Jones"
      ]

instance IsTool DuckDuckGoSearch where
  toolDescription _ = Just "DuckDuckGo Instant Answer: returns a short abstract for well-known named entities (people, places) that have a Wikipedia article. Returns no result for vague or multi-word queries — use wikipedia_search instead."

-- | DuckDuckGo Instant Answer API response (partial).
data DDGResponse = DDGResponse
  { abstractText :: Maybe Text
  , abstractURL :: Maybe Text
  , heading :: Maybe Text
  , answer :: Maybe Text
  }

instance Aeson.FromJSON DDGResponse where
  parseJSON = withObject "DDGResponse" $ \o ->
    DDGResponse
      <$> o .:? "AbstractText"
      <*> o .:? "AbstractURL"
      <*> o .:? "Heading"
      <*> o .:? "Answer"

-- | Query DuckDuckGo Instant Answer API, return AbstractText (+ URL) or Answer.
duckDuckGoSearchHandler :: ToolHandler DuckDuckGoSearch
duckDuckGoSearchHandler = ToolHandler $ \(DuckDuckGoSearch q) -> do
  result <- try @SomeException $ runReq defaultHttpConfig $ do
    r <-
      req
        GET
        (https "api.duckduckgo.com")
        NoReqBody
        bsResponse
        ( Req.queryParam "q" (Just q)
            <> Req.queryParam "format" (Just ("json" :: Text))
            <> Req.queryParam "no_html" (Just ("1" :: Text))
            <> Req.queryParam "skip_disambig" (Just ("1" :: Text))
            <> userAgentHeader
        )
    pure $ responseBody r
  case result of
    Left ex -> pure $ Left ("DuckDuckGoSearch error: " <> T.pack (show ex))
    Right bs ->
      case Aeson.decodeStrict bs of
        Nothing -> pure $ Right "DuckDuckGoSearch: could not parse response"
        Just ddg ->
          pure $ Right $ case answer ddg of
            Just a | not (T.null a) -> a
            _ -> case abstractText ddg of
              Just t
                | not (T.null t) ->
                    let url = maybe "" ("\nURL: " <>) (abstractURL ddg)
                     in T.take 500 t <> url
              _ -> case heading ddg of
                Just h | not (T.null h) -> "Search result: " <> h
                _ -> "No result found for: " <> q

-- * WikipediaSearch

-- | A Wikipedia article search query.
newtype WikipediaSearch = WikipediaSearch
  { wikiQuery :: Text
  -- ^ The search query string.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- We need a new declaration group so TH can reify the type defined above.
$(pure [])

data WikipediaSearchProperty
  = -- | Must not be empty
    WikipediaSearchQueryNotEmpty
  | -- | Only alphanumerics, spaces, basic punctuation
    WikipediaSearchQuerySafeChars
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

instance Describe WikipediaSearch where
  type Property WikipediaSearch = WikipediaSearchProperty
  describeType = $(deriveDescribeType ''WikipediaSearch)
  describeProperties _ WikipediaSearchQueryNotEmpty =
    Just "The search query must not be empty."
  describeProperties _ WikipediaSearchQuerySafeChars =
    Just "The search query must contain only alphanumerics, spaces, and basic punctuation (\".,'-\"). No newlines, no HTML, no shell special characters."
  propertyHolds (WikipediaSearch q) WikipediaSearchQueryNotEmpty = not (T.null q)
  propertyHolds (WikipediaSearch q) WikipediaSearchQuerySafeChars =
    T.all (\c -> isAlphaNum c || c `elem` (" .,'-" :: String)) q
  examples _ =
    S.fromList
      [ WikipediaSearch "Haskell monad tutorial"
      , WikipediaSearch "functional programming"
      ]

instance IsTool WikipediaSearch where
  toolDescription _ = Just "Returns up to 5 Wikipedia article titles and URLs matching the query."

{- | Query the Wikipedia OpenSearch API, return article titles and URLs.
Works for any query; use 'webFetchHandler' to read a returned URL.
-}
wikipediaSearchHandler :: ToolHandler WikipediaSearch
wikipediaSearchHandler = ToolHandler $ \(WikipediaSearch q) -> do
  result <- try @SomeException $ runReq defaultHttpConfig $ do
    r <-
      req
        GET
        (https "en.wikipedia.org" /: "w" /: "api.php")
        NoReqBody
        bsResponse
        ( Req.queryParam "action" (Just ("opensearch" :: Text))
            <> Req.queryParam "search" (Just q)
            <> Req.queryParam "limit" (Just ("5" :: Text))
            <> Req.queryParam "format" (Just ("json" :: Text))
            <> userAgentHeader
        )
    pure $ responseBody r
  case result of
    Left ex -> pure $ Left ("WikipediaSearch error: " <> T.pack (show ex))
    Right bs ->
      -- OpenSearch response: [query, [titles], [descriptions], [urls]]
      case Aeson.decodeStrict bs of
        Just (Aeson.Array arr)
          | length arr == 4
          , Just titles <- decodeTexts (arr V.! 1)
          , Just urls <- decodeTexts (arr V.! 3)
          , not (null titles) ->
              pure $
                Right $
                  T.intercalate
                    "\n"
                    [t <> " — " <> u | (t, u) <- zip titles urls]
        _ -> pure $ Right ("No Wikipedia results for: " <> q)
  where
    decodeTexts (Aeson.Array v) = Just [t | Aeson.String t <- V.toList v]
    decodeTexts _ = Nothing
