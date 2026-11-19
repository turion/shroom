{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Control.Monad.Prompt.Generic
Description : Prompt a record or sum type part by part, with @deriving via Generically@.

Prompt a type part by part, after choosing its constructor if it has several,
by deriving 'Promptable' via t'Generically'.

A flat 'Promptable' asks for the whole value in one request. The strategy here
instead asks for a record's fields one at a time, each seeing the earlier ones,
so that parts that must fit together are built in one train of thought. It then
asks the model to 'Control.Monad.Prompt.Effect.improve' the assembled value, in
case the parts do not fit after all.

== An example

A musician has a name and a city they are from. A musician from a city that
they have nothing to do with is wrong, though each part alone looks fine. Here
is the type, derived part by part:

@
-- | The name of a city.
newtype City = City Text
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

\$(deriveDescribable ''City)

instance Surveyable City

-- A leaf is flat: one request.
instance Promptable City

-- | A musician.
data Musician = Musician
  { name :: Text
  -- ^ The musician's full name.
  , city :: City
  -- ^ The city the musician is from.
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

\$(deriveDescribable ''Musician)

instance Surveyable Musician

deriving via Generically Musician instance Promptable Musician
@

Here @SOP@ is "Generics.SOP". The example needs the packages @shroom@,
@generics-sop@, @aeson@, @openapi3@ and @text@, and the GHC flag @-haddock@ for
the module that declares the types: the descriptions are read from their
Haddock comments (see "Data.Shroom.Class.TH"). A leaf type that a field uses is
derived before the type that has the field.

A program asks for a musician with @prompt \@Musician@, as for any 'Promptable'
type. The request then goes through these steps:

1. The model is shown the request for a @Musician@ and answers with a cue for
   all parts, for example \"Make it a classical composer\".
2. Each part is requested on its own, and sees the request, the cue and the
   parts before it. The name comes out as @\"Wolfgang Amadeus Mozart\"@. The city,
   though it sees the name, may come out as @City \"Eisenach\"@: a plausible
   city for a composer, but not Mozart's.
3. shroom assembles the parts into
   @Musician \"Wolfgang Amadeus Mozart\" (City \"Eisenach\")@.
4. The model is shown this assembled value and asked to improve it, which means
   either to keep it or to return a corrected one.
5. The model notices that the name and the city do not fit, since Bach was born
   in Eisenach and Mozart in Salzburg. It corrects the value to
   @Musician \"Johann Sebastian Bach\" (City \"Eisenach\")@ or to
   @Musician \"Wolfgang Amadeus Mozart\" (City \"Salzburg\")@.

Without the last two steps, the mismatch of step 3 would stay. That is why the
strategy ends with 'Control.Monad.Prompt.Effect.improve'.

A sum is derived the same way; its strategy first asks which constructor to
build.

Only the class method 'Control.Monad.Prompt.Promptable.prompt' runs the strategy, directly or through
the instance of a field. The flat 'Control.Monad.Prompt.Effect.promptSingle' and
'Control.Monad.Prompt.Effect.improve' send one request for the whole value and
never run it.

== The strategy

'Control.Monad.Prompt.Promptable.prompt' for @t'Generically' a@ builds an @a@ in the following steps, all
inside one snapshot of the context:

1. Only for a type with several constructors: the model is told it is about to
   build a value of @a@, described by 'description', and is asked for a
   t'ConstructorChoice', an enum of the constructor names.
2. Unless the constructor has no fields: the model is asked for a t'Cue', a short,
   specific steer that every field will follow: a tone or a setting that the first
   field's request would otherwise leave to chance, and the later ones inherit.
3. For each field in order, a message names the field (a record's field name,
   otherwise the constructor's name and the field's position), and the field
   type's own 'Control.Monad.Prompt.Promptable.prompt' is called. Its answer stays in the context, so
   each field sees the earlier ones.
4. The fields are assembled into a draft and go through
   'Control.Monad.Prompt.Effect.improve', which checks it against the type's own
   'Property' (see 'Surveyable') and either keeps it or returns a corrected one.
5. The context is restored to the snapshot, and one 'AssistantMessage' holding the
   final value's JSON is appended, as after a flat 'Control.Monad.Prompt.Promptable.prompt'.

Note that a field type whose own @prompt@ scopes its context leaves no answer
behind; the strategy then appends the answer, whenever the context did not grow
or does not end in an 'AssistantMessage'. It cannot tell whether a trailing
'AssistantMessage' really is the field's answer, so an override that drops its
answer yet appends some other 'AssistantMessage' last goes unnoticed.

== Cost

A record with @n > 0@ fields costs @n + 2@ requests (cue, fields, improve), where
a flat request costs 1: the @Musician@ above costs 4. A constructor of a type with
several constructors costs one more for the choice, and one more for every retry.
Derive the leaves flat, with a bare @instance Promptable@, as asking for a scalar
part by part would pay a cue and an @improve@ for nothing; use @Generically@ for
the records and sums whose parts must cohere.

== The limits of the strategy

A field of type @[a]@, @NonEmpty a@, @Set a@ or @Maybe a@ is prompted whole, in one
request for the container, and the properties of its elements are neither checked
nor told to the model, as for any container field (see "Data.Shroom.Class").
A recursive type needs the opt-out that "Data.Shroom.Class" describes. Anthropic's
structured outputs do not support recursive schemas, so such a type fails against
Claude even with it.

The schema that 'improve' sends for a sum is the one
'Control.Monad.Prompt.Backend.schemaWithDefs' builds from its 'ToSchema':
normalised for structured outputs, so that its @oneOf@ becomes @anyOf@, and
self-contained.
-}
module Control.Monad.Prompt.Generic (
  Cue (..),
  ConstructorChoice,
) where

-- base
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generically (..))

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), withText)

-- openapi3
import Data.OpenApi (NamedSchema (..), OpenApiType (..), Schema (..), ToSchema (..))

-- generics-sop
import Generics.SOP (All, All2, Code, ConstructorInfo, I (..), K (..), NP (..), NS (..), SList (..), SOP (..), constructorInfo, constructorName, datatypeInfo, datatypeName, sList, to)
import Generics.SOP qualified as SOP

-- effectful
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, get, put)

-- shroom
import Control.Monad.Prompt.Backend (ContextItem (..))
import Control.Monad.Prompt.Effect (Prompt, addContextItem, improve)
import Control.Monad.Prompt.Promptable (Promptable)
import Control.Monad.Prompt.Promptable qualified as Promptable
import Control.Monad.Prompt.Schema (encodeJson)
import Data.Shroom.Class

-- * The cue

{- | A short, specific steer that every field of a value under construction
follows: a tone, a setting, a constraint. The model is asked for it before the
first field, once the constructor is known.
-}
newtype Cue = Cue Text
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToSchema)

instance Describable Cue where
  describeType _ = "A short, specific steer, in one or two sentences: the tone, setting or constraint that every field of the value being built should follow."

instance Surveyable Cue where
  type Property Cue = Void

-- Flat: one request for the whole value.
instance Promptable Cue

-- * The constructor choice

{- | The constructor of an @a@ to build, one of the constructors of @a@. When building
an @a@ part by part, the model is asked for it first, and only for a type with several
constructors.

On the wire it is a string, one of the constructor names, and nothing else is
accepted; its schema is a string enum named after @a@, so the choices of two
sum types do not collide.
-}
newtype ConstructorChoice a = ConstructorChoice (NS (K ()) (Code a))

-- | The names of the constructors of @a@, in order.
constructorNames :: forall a. (SOP.HasDatatypeInfo a) => Proxy a -> [Text]
constructorNames prx = go (constructorInfo (datatypeInfo prx))
  where
    go :: NP ConstructorInfo xss -> [Text]
    go Nil = []
    go (c :* cs) = T.pack (constructorName c) : go cs

-- | The name of the chosen constructor.
choiceName :: NP ConstructorInfo xss -> NS (K ()) xss -> Text
choiceName (c :* _) (Z _) = T.pack (constructorName c)
choiceName (_ :* cs) (S choice) = choiceName cs choice
choiceName Nil choice = case choice of {}

-- | The choice naming a constructor, if there is one of that name.
choiceFromName :: NP ConstructorInfo xss -> Text -> Maybe (NS (K ()) xss)
choiceFromName Nil _ = Nothing
choiceFromName (c :* cs) name
  | T.pack (constructorName c) == name = Just (Z (K ()))
  | otherwise = S <$> choiceFromName cs name

instance (SOP.HasDatatypeInfo a) => ToJSON (ConstructorChoice a) where
  toJSON (ConstructorChoice choice) = String (choiceName (constructorInfo (datatypeInfo (Proxy @a))) choice)

instance (SOP.HasDatatypeInfo a) => FromJSON (ConstructorChoice a) where
  parseJSON = withText "ConstructorChoice" $ \name ->
    case choiceFromName (constructorInfo (datatypeInfo (Proxy @a))) name of
      Just choice -> pure (ConstructorChoice choice)
      Nothing -> fail ("not the name of a constructor: " <> T.unpack name <> ", expected one of " <> T.unpack (T.intercalate ", " (constructorNames (Proxy @a))))

instance (Typeable a, SOP.HasDatatypeInfo a) => ToSchema (ConstructorChoice a) where
  declareNamedSchema _ =
    pure $
      NamedSchema
        (Just (T.pack (datatypeName (datatypeInfo (Proxy @a))) <> "Choice"))
        mempty
          { _schemaType = Just OpenApiString
          , _schemaEnum = Just (String <$> constructorNames (Proxy @a))
          }

instance (SOP.HasDatatypeInfo a, Describable a) => Describable (ConstructorChoice a) where
  describeType _ =
    "A choice of which kind of the type described below to build, made by naming one of its constructors, exactly as written.\n"
      <> describeType (Proxy @a)

instance (SOP.HasDatatypeInfo a, Describable a) => Surveyable (ConstructorChoice a) where
  type Property (ConstructorChoice a) = Void

-- Flat: one request for the whole value.
instance (Typeable a, SOP.HasDatatypeInfo a, Describable a) => Promptable (ConstructorChoice a)

-- * The strategy

{- | Build an @a@ part by part: choose its constructor if it has several, then
prompt its fields in order and improve the assembled draft. The context ends as
the caller's plus the final value. The 'Promptable' instance for t'Generically'
wraps it.
-}
promptGeneric ::
  forall a es.
  ( Prompt :> es
  , State [ContextItem] :> es
  , Surveyable a
  , ToSchema a
  , FromJSON a
  , SOP.Generic a
  , SOP.HasDatatypeInfo a
  , All2 Promptable (Code a)
  ) =>
  Eff es a
promptGeneric = do
  (saved :: [ContextItem]) <- get
  let parent = description (Proxy @a)
      choose = (\(ConstructorChoice choice) -> choice) <$> Promptable.prompt @(ConstructorChoice a)
  built <- promptConstructors parent choose (constructorInfo (datatypeInfo (Proxy @a)))
  let draft = to (SOP built)
  final <- fromMaybe draft <$> improve draft
  put (saved <> [AssistantMessage (encodeJson final)])
  pure final

{- | Build the fields of one of the constructors. A single constructor is the
type itself and needs no choice; with several, the model is first told what is
being built and asked which constructor (the action @choose@ does the asking), and
then the fields of that constructor are built. @parent@ is the description of the
whole type.
-}
promptConstructors ::
  forall xss es.
  (Prompt :> es, State [ContextItem] :> es, All2 Promptable xss) =>
  Text ->
  Eff es (NS (K ()) xss) ->
  NP ConstructorInfo xss ->
  Eff es (NS (NP I) xss)
promptConstructors parent choose constructors = case constructors of
  constructor :* Nil -> Z <$> promptConstructor parent False constructor
  _ -> do
    -- Persistent context, not 'withContext': that would drop every answer.
    addContextItem $
      UserMessage $
        "You are about to build a value of the type described below. First choose which of its constructors to build it with; then its fields are built one by one.\n"
          <> parent
    choice <- choose
    promptChosen parent constructors choice

-- | Build the fields of the chosen one of several constructors.
promptChosen ::
  forall xss es.
  (Prompt :> es, State [ContextItem] :> es, All2 Promptable xss) =>
  Text ->
  NP ConstructorInfo xss ->
  NS (K ()) xss ->
  Eff es (NS (NP I) xss)
promptChosen parent (constructor :* _) (Z _) = Z <$> promptConstructor parent True constructor
promptChosen parent (_ :* constructors) (S choice) = S <$> promptChosen parent constructors choice
promptChosen _ Nil choice = case choice of {}

{- | The product strategy for one constructor: a message and the t'Cue', then its
fields. A constructor without fields is built at once. The flag says whether the
constructor was chosen among several, which the messages then say.
-}
promptConstructor ::
  forall xs es.
  (Prompt :> es, State [ContextItem] :> es, All Promptable xs) =>
  Text ->
  Bool ->
  ConstructorInfo xs ->
  Eff es (NP I xs)
promptConstructor parent chosen constructor = case sList :: SList xs of
  SNil -> pure Nil
  SCons -> do
    addContextItem $
      UserMessage $
        if chosen
          then "You chose the constructor " <> T.pack (constructorName constructor) <> ". Before its first field, give one short, specific steer that every field will follow."
          else
            "You are about to build a value of the type described below, field by field.\n"
              <> parent
              <> "\nBefore the first field, give one short, specific steer that every field will follow."
    _cue <- Promptable.prompt @Cue
    promptFields (fieldLabel chosen constructor) 0

{- | Prompt each field of one constructor in order, each through its own type's
'Control.Monad.Prompt.Promptable.prompt', after a message naming it. The label function says how to
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

{- | The name of the field at a position: the record field's, else the
constructor's and the position. When the constructor was chosen among several,
a record field's name is followed by the constructor's as well.
-}
fieldLabel :: Bool -> ConstructorInfo xs -> Int -> Text
fieldLabel chosen constructor i = case fieldNameAt i constructor of
  Just name
    | chosen -> "the field " <> name <> " of constructor " <> conName
    | otherwise -> "the field " <> name
  Nothing -> "field " <> T.pack (show (i + 1)) <> " of constructor " <> conName
  where
    conName = T.pack (constructorName constructor)

-- * The instances

-- | The schema of @a@.
instance (ToSchema a) => ToSchema (Generically a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a)

-- | Prompt an @a@ part by part, through @deriving via Generically@.
instance
  ( Surveyable a
  , ToSchema a
  , FromJSON a
  , ToJSON (Generically a)
  , FromJSON (Generically a)
  , SOP.Generic a
  , SOP.HasDatatypeInfo a
  , All2 Promptable (Code a)
  ) =>
  Promptable (Generically a)
  where
  prompt = Generically <$> promptGeneric @a
