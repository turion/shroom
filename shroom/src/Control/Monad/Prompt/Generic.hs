{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Prompt a single-constructor type field by field: @deriving via 'Generically'@.

@
data Musician = Musician
  { name :: Text
  , instrument :: Text
  , bio :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

\$(deriveDescribable ''Musician)

instance Surveyable Musician

deriving via Generically Musician instance Promptable Musician
@

The type needs the same instances as for a flat 'Promptable' ('Surveyable',
'ToSchema', 'FromJSON'), and besides them the @generics-sop@ classes
@SOP.Generic@ and @SOP.HasDatatypeInfo@ (derivable with @deriving anyclass@, with
@generics-sop@ among your @build-depends@), and a 'Promptable' instance for
every field type. Only the class method
'Control.Monad.Prompt.Promptable.prompt' takes the strategy: the flat
'Control.Monad.Prompt.Effect.prompt' is unchanged, and so is
'Control.Monad.Prompt.Effect.improve'.

== The strategy

'prompt' for @'Generically' a@ builds an @a@ in several requests, all inside one
snapshot of the context:

1. A message tells the model it is about to build a value of @a@, described by
   'description', field by field, and the model is asked for a 'Cue': a short,
   specific steer that every field will follow. The reply stays in the context.
2. For each field in order, a message names the field (a record's field name,
   otherwise the constructor's name and the field's position), and the field
   type's __own__ 'Control.Monad.Prompt.Promptable.prompt' is called. Its
   answer stays in the context, so each field sees the earlier ones. A field type
   whose own @prompt@ scopes its context (and so leaves no answer behind) has its
   answer appended by the strategy: whenever the context did not grow, or does not
   end in an 'AssistantMessage'. The strategy cannot tell whether a trailing
   'AssistantMessage' is really that field's answer, so an override that drops
   its answer yet appends some other 'AssistantMessage' last goes unnoticed.
3. The fields are assembled into an @a@, a /draft/.
4. The draft goes through 'Control.Monad.Prompt.Effect.improve', unconditionally. A
   @fine@ verdict keeps the draft, an @improved@ one replaces it. A draft that
   violates a property of @a@ is never kept on a @fine@ verdict: 'improve' retries
   that.
5. The context is restored to the snapshot, and one 'AssistantMessage' holding the
   final value's JSON is appended: the caller's context ends as after a flat
   'Control.Monad.Prompt.Promptable.prompt', that is, what it was plus the value.

That is @n + 2@ requests for a record with @n@ fields (cue, fields, improve),
more for a field that is itself generic, and one more for every retry.

== Which types

A type with exactly one constructor. A type with several constructors is rejected
at compile time (GHC cannot solve @Code a ~ '[xs]@); choosing a constructor first
is not implemented yet.

== Orphan instances

This module holds two orphans, each commented where it is defined.
-}
module Control.Monad.Prompt.Generic (
  Cue (..),
) where

-- base
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import GHC.Generics (Generically (..))

-- text
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)

-- aeson
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Text (encodeToLazyText)

-- openapi3
import Data.OpenApi (ToSchema (..))

-- generics-sop
import Generics.SOP (All, Code, ConstructorInfo, I (..), NP (..), NS (..), SList (..), SOP (..), constructorInfo, constructorName, datatypeInfo, sList, to)
import Generics.SOP qualified as SOP

-- effectful
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, get, put)

-- shroom
import Control.Monad.Prompt.Backend (ContextItem (..))
import Control.Monad.Prompt.Effect (Prompt, addContextItem, improve)
import Control.Monad.Prompt.Promptable (Promptable)
import Control.Monad.Prompt.Promptable qualified as Promptable
import Data.Shroom.Class

-- * The cue

{- | A short, specific steer that every field of a value under construction
follows: a tone, a setting, a constraint. The first thing the strategy asks the
model for.
-}
newtype Cue = Cue Text
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToSchema)

instance Describable Cue where
  describeType _ = "A short, specific steer, in one or two sentences: the tone, setting or constraint that every field of the value being built should follow."

instance Surveyable Cue where
  type Property Cue = Void

-- | Flat, like the base types.
instance Promptable Cue

-- * The strategy

{- | The strategy for a type with one constructor, see the module header. Written
for @a@ itself; the 'Promptable' instance for 'Generically' wraps it.

The single-constructor restriction is the @Code a ~ '[xs]@ constraint. A sum case
would add the choice of a constructor in front of 'promptFields', which is
already written per constructor.
-}
promptProduct ::
  forall a xs es.
  ( Prompt :> es
  , State [ContextItem] :> es
  , Surveyable a
  , ToSchema a
  , FromJSON a
  , SOP.Generic a
  , SOP.HasDatatypeInfo a
  , Code a ~ '[xs]
  , All Promptable xs
  ) =>
  Eff es a
promptProduct = do
  (saved :: [ContextItem]) <- get
  -- Persistent context, not 'withContext': that would drop every answer.
  addContextItem $
    UserMessage $
      "You are about to build a value of the type described below, field by field.\n"
        <> description (Proxy @a)
        <> "\nBefore the first field, give one short, specific steer that every field will follow."
  _cue <- Promptable.prompt @Cue
  let constructor = singleConstructor (constructorInfo (datatypeInfo (Proxy @a)))
  fields <- promptFields (fieldLabel constructor) 0
  let draft = to (SOP (Z fields))
  final <- fromMaybe draft <$> improve draft
  put (saved <> [AssistantMessage (encodeJson final)])
  pure final

{- | Prompt each field of one constructor in order, each through its own type's
'Promptable.prompt', after a message naming it. The label function says how to
name the field at a (zero-based) position.
-}
promptFields ::
  forall xs es.
  (Prompt :> es, State [ContextItem] :> es, All Promptable xs) =>
  (Int -> Text) ->
  Int ->
  Eff es (NP I xs)
promptFields label i = case sList :: SList xs of
  SNil -> pure Nil
  SCons -> do
    addContextItem (UserMessage ("Next, produce " <> label i <> " of the value."))
    (before :: [ContextItem]) <- get
    value <- Promptable.prompt
    (after :: [ContextItem]) <- get
    -- A field whose own @prompt@ scopes its context leaves no answer behind;
    -- later fields must still see it. An answer is taken to be a context that
    -- grew and ends in an 'AssistantMessage'.
    unless (length after > length before && endsInAssistantMessage after) $
      addContextItem (AssistantMessage (encodeJson value))
    rest <- promptFields label (i + 1)
    pure (I value :* rest)

-- | Whether the last item of a context is an 'AssistantMessage'.
endsInAssistantMessage :: [ContextItem] -> Bool
endsInAssistantMessage ctx = case reverse ctx of
  AssistantMessage _ : _ -> True
  _ -> False

-- | The name of the field at a position: the record field's, else the constructor's and the position.
fieldLabel :: ConstructorInfo xs -> Int -> Text
fieldLabel constructor i = case fieldNameAt i constructor of
  Just name -> "the field " <> name
  Nothing -> "field " <> T.pack (show (i + 1)) <> " of constructor " <> T.pack (constructorName constructor)

singleConstructor :: NP f '[xs] -> f xs
singleConstructor (x :* Nil) = x

encodeJson :: (ToJSON v) => v -> Text
encodeJson = toStrict . encodeToLazyText

-- * The instances

{- | Orphan. @openapi3@ has no 'ToSchema' for 'Generically', and it belongs there
upstream; until then it is here. It forwards to @a@: the strategy only ever
uses the schema of @a@ itself, and this instance exists to satisfy the
superclass of 'Promptable'.
-}
instance (ToSchema a) => ToSchema (Generically a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a)

{- | Orphan. It cannot sit next to the class in
"Control.Monad.Prompt.Promptable": the strategy calls that class's own
'Promptable.prompt' on every field and 'improve' from
"Control.Monad.Prompt.Effect", and keeping the strategy in a module of its own is
worth one orphan. 'Describable' and 'Surveyable' for 'Generically' are not
orphans: they are in "Data.Shroom.Class".

The method is what @deriving via@ coerces; the context is mostly about @a@,
and @ToJSON (Generically a)@ and @FromJSON (Generically a)@ are @aeson@'s
generic ones, needed by the superclasses.
-}
instance
  ( Surveyable a
  , ToSchema a
  , FromJSON a
  , ToJSON (Generically a)
  , FromJSON (Generically a)
  , SOP.Generic a
  , SOP.HasDatatypeInfo a
  , Code a ~ '[xs]
  , All Promptable xs
  ) =>
  Promptable (Generically a)
  where
  prompt = Generically <$> promptProduct @a
