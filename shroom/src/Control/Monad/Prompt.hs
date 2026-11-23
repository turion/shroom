{- |
Module      : Control.Monad.Prompt
Description : Ask a language model for a typed Haskell value.

'prompt' asks a language model for a value of any 'Promptable' type. The model is
told the type's description, properties and examples (see "Data.Shroom.Class"), and
its answer is parsed and checked against them, with a retry when it fails.

Programs that chain requests, add context or offer tools are written in
"Control.Monad.Prompt.Effect". For example, the program below gives additional
context, asks for a user and then asks for a second user who differs from the
first. The description of @User@ is read from the
Haddock comments, so the module that declares @User@ needs the GHC flag
@-haddock@ (see "Data.Shroom.Class.TH").

@
-- | A user with a name and an email address.
data User = User
  { userName :: Text
  -- ^ The user's full name.
  , userEmail :: Text
  -- ^ The user's email address.
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

\$(deriveDescribable ''User)

instance Surveyable User

instance Promptable User

signUpTwice :: (Prompt :> es, State [ContextItem] :> es) => Eff es (User, User)
signUpTwice = do
  context \"Alice Smith signed up with alice\@example.org.\"
  first <- prompt \@User
  context \$ \"A second user signed up, who is not \" <> userName first <> \".\"
  second <- prompt \@User
  pure (first, second)
@

To run the program against a backend, see "Control.Monad.Prompt.Backend" and the
<https://hackage.haskell.org/package/shroom-baikai shroom-baikai> package.
-}
module Control.Monad.Prompt (module Control.Monad.Prompt.Promptable) where

-- shroom
import Control.Monad.Prompt.Promptable
