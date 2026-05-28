module Control.Monad.Prompt (module Control.Monad.Prompt) where

-- base
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (Foldable (..))
import Data.Proxy (Proxy (..))

-- mtl
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Reader (MonadReader (..))

-- transformers
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT (..))

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- witherable
import Witherable ((<&?>))

-- claude
import Claude.V1
import Claude.V1.Messages

-- operational
import Control.Monad.Operational
import Data.Aeson (FromJSON, eitherDecodeStrictText)
import Data.OpenApi (ToSchema)

-- shroom
import Data.Describe
import Data.Describe qualified as D

data Prompt a where
  Context :: Text -> Prompt ()
  Prompt :: (ToSchema a, FromJSON a, Describe a) => Prompt a

newtype PromptT m a = PromptT {getPromptT :: ProgramT Prompt m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

context :: Text -> PromptT m ()
context txt = PromptT $ singleton $ Context txt

prompt :: (ToSchema a, FromJSON a, Describe a) => PromptT m a
prompt = PromptT $ singleton Prompt

newtype PromptResultT m a = PromptResultT {getPromptResultT :: ReaderT PromptConfig (ExceptT Text m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader PromptConfig, MonadError Text)

instance MonadTrans PromptResultT where
  lift = PromptResultT . lift . lift

data PromptConfig = PromptConfig
  { apiKey :: Text
  , model :: Text
  }

hoistOperational :: (Monad n, Monad m) => (forall x. m x -> n x) -> ProgramT instr m a -> ProgramT instr n a
hoistOperational morph = join . lift . fmap (hoistOperational morph . unviewT) . morph . viewT

runPromptT :: (MonadIO m) => PromptT m a -> PromptResultT m a
runPromptT p = do
  config <- ask
  clientEnv <- liftIO $ getClientEnv "https://api.anthropic.com"

  loop (getPromptT p) (makeMethods clientEnv config.apiKey (Just "2023-06-01")) config
  where
    mkProxy :: Prompt a -> Proxy a
    mkProxy _ = Proxy

    loop initialPrompt methods config = go "" initialPrompt
      where
        go :: (MonadIO m) => Text -> ProgramT Prompt m a -> PromptResultT m a
        go pastContext currentProgramT = do
          command <- lift $ viewT currentProgramT
          case command of
            Return a -> pure a
            Context txt :>>= k -> do
              go (pastContext <> "\n" <> txt) (k ())
            currentPrompt@Prompt :>>= k -> do
              MessageResponse {content} <-
                liftIO $
                  methods.createMessage
                    _CreateMessage
                      { model = config.model
                      , messages =
                          [ Message
                              { role = User
                              , content = [Content_Text {text = pastContext <> D.description (mkProxy currentPrompt), cache_control = Nothing}]
                              , cache_control = Nothing
                              }
                          ]
                      , max_tokens = 1024
                      }

              let result =
                    toList $
                      content <&?> \case
                        ContentBlock_Text {text = t} -> Just t
                        _ -> Nothing
              case eitherDecodeStrictText (T.unlines result) of
                Left err -> throwError $ T.pack err
                Right a -> go "" (k a)

runPromptResultTWith :: PromptConfig -> PromptResultT m a -> m (Either Text a)
runPromptResultTWith config (PromptResultT r) = runExceptT $ runReaderT r config
