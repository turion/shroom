{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Built-in web tools: 'WebFetch' and 'WebSearch'.

Use 'webFetchHandler' and 'webSearchHandler' as ready-made 'ToolHandler'
values in your 'runPromptT' call:

@
result <- runPromptResultTWith cfg $
  runPromptT defaultPromptConfig
    (webFetchHandler :* webSearchHandler :* Nil)
    myProgram
@
-}
module Control.Monad.Prompt.Tool.Web (module Control.Monad.Prompt.Tool.Web) where

-- base
import Control.Exception (SomeException, try)

-- bytestring
import Data.ByteString.Char8 qualified as BS

-- text
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

-- aeson
import Data.Aeson (FromJSON, ToJSON, withObject, (.:?))
import Data.Aeson qualified as Aeson
import GHC.Generics (Generic)

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
 )
import Network.HTTP.Req qualified as Req

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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- We need a new declaration group so TH can reify the type defined above.
$(pure [])

instance Describe WebFetch where
  describeType = $(deriveDescribeType ''WebFetch)

instance IsTool WebFetch where
  toolDescription _ = Just "Returns up to 2000 characters of the page body."

-- | Perform an HTTP GET, strip HTML tags, truncate to 2000 characters.
webFetchHandler :: ToolHandler WebFetch
webFetchHandler = ToolHandler $ \(WebFetch url) -> do
  result <- try @SomeException $ runReq defaultHttpConfig $ do
    uri <- mkURI url
    case useHttpsURI uri of
      Just (u, opts) -> do
        r <- req GET u NoReqBody bsResponse opts
        pure $ responseBody r
      Nothing ->
        case useHttpURI uri of
          Just (u, opts) -> do
            r <- req GET u NoReqBody bsResponse opts
            pure $ responseBody r
          Nothing -> pure $ BS.pack ("WebFetch: could not parse URL: " <> T.unpack url)
  case result of
    Left ex -> pure $ Left ("WebFetch error: " <> T.pack (show ex))
    Right bs ->
      let txt = TE.decodeUtf8Lenient bs
          -- Strip HTML tags using tagsoup
          stripped = innerText (parseTags txt :: [Tag Text])
          -- Collapse whitespace
          cleaned = T.unwords . filter (not . T.null) . T.words $ stripped
       in pure $ Right (T.take 2000 cleaned)

-- * WebSearch

{- | A web search query.
The LLM provides a search string; the handler returns a brief summary.
-}
newtype WebSearch = WebSearch
  { searchQuery :: Text
  -- ^ The search query string.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- We need a new declaration group so TH can reify the type defined above.
$(pure [])

instance Describe WebSearch where
  describeType = $(deriveDescribeType ''WebSearch)

instance IsTool WebSearch where
  toolDescription _ = Just "Returns a short summary from DuckDuckGo."

-- | DuckDuckGo Instant Answer API response (partial).
data DDGResponse = DDGResponse
  { abstractText :: Maybe Text
  , heading :: Maybe Text
  , answer :: Maybe Text
  }

instance Aeson.FromJSON DDGResponse where
  parseJSON = withObject "DDGResponse" $ \o ->
    DDGResponse
      <$> o .:? "AbstractText"
      <*> o .:? "Heading"
      <*> o .:? "Answer"

-- | Query DuckDuckGo Instant Answer API, return AbstractText or Answer.
webSearchHandler :: ToolHandler WebSearch
webSearchHandler = ToolHandler $ \(WebSearch q) -> do
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
        )
    pure $ responseBody r
  case result of
    Left ex -> pure $ Left ("WebSearch error: " <> T.pack (show ex))
    Right bs ->
      case Aeson.decodeStrict bs of
        Nothing -> pure $ Right "WebSearch: could not parse response"
        Just ddg ->
          pure $ Right $ case answer ddg of
            Just a | not (T.null a) -> a
            _ -> case abstractText ddg of
              Just t | not (T.null t) -> T.take 500 t
              _ -> case heading ddg of
                Just h | not (T.null h) -> "Search result: " <> h
                _ -> "No result found for: " <> q
