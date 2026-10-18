{-# LANGUAGE TemplateHaskell #-}

{- | Template Haskell utilities for deriving 'Describable' instances from Haddock
documentation comments.
-}
module Control.Monad.Prompt.TH (deriveDescribable, deriveDescribableExp) where

-- base
import Data.Kind qualified as Kind
import Data.List (nub)
import Data.Typeable (Typeable)

-- template-haskell
import Language.Haskell.TH

-- text
import Data.Text (pack)

-- shroom
import Data.Shroom.Class (Describable (describeType, referencedTypes), reach)

{- | Generate a complete 'Describable' instance for a type, deriving
'describeType' from its Haddock documentation comment and
'referencedTypes' from its fields.  Use this when
you don't need to customise the description:

@
-- | A user with a name and an email address.
data User = User { userName :: Text, userEmail :: Text }

\$(deriveDescribable ''User)
@

This generates:

@
instance Describable User where
  describeType = \_ -> pack "A user with a name and an email address.\\n..."
  referencedTypes _ = reach \@Text <> (reach \@Text <> mempty)
@

'referencedTypes' is the concatenation, in declaration order, of @reach \@t@ for
the type @t@ of every field of every constructor, record and positional alike. So
the types of the fields reach the model through 'Data.Shroom.Class.description':
each is described once, in a section after the examples, however often and however
deeply it is used.

__Every field type must have a 'Describable' instance__, from an earlier splice or
the same one; a field type without one is a compile-time error (\"No instance for
'Describable' …\"). Splice order therefore matters. Mutually recursive types
are derived in a single splice, @\$(concat \<$\> traverse deriveDescribable [''A, ''B])@; a
self-recursive type needs nothing special.

A parameterised type derives too, with one @'Describable' a@ constraint for each type
variable of kind @Type@:

@
-- | One of two alternatives.
data Choice a = Pick a | Skip

\$(deriveDescribable ''Choice)
@

generates @instance ('Describable' a) => 'Describable' (Choice a)@, where a field of type
@a@ contributes @reach \@a@, so that @Choice Fuel@ makes @Fuel@ reachable.

A type variable of any other kind (a @Symbol@ or @Nat@ unit, a dimension, or an
unannotated phantom parameter, which GHC2024 makes poly-kinded) can never be the
type of a field, so it needs no 'Describable'. It is annotated with its kind in the
instance head and needs only @Typeable@, as does each kind variable its kind
mentions:

@
-- | An amount in some unit.
newtype Measure (unit :: Symbol) = Measure Double

-- | A value tagged by a phantom parameter.
newtype Tagged a = Tagged Double

\$(deriveDescribable ''Measure)
\$(deriveDescribable ''Tagged)
@

generates @instance 'Typeable' unit => 'Describable' (Measure (unit :: Symbol))@ and
@instance ('Typeable' k, 'Typeable' a) => 'Describable' (Tagged (a :: k))@. The
generated code uses type applications and the instance head's type
variables, so the module needs @TypeApplications@ and @ScopedTypeVariables@ (both
in @GHC2021@ and @GHC2024@).

To override 'describeType' manually, write the 'Describable' instance by hand.
You can use 'deriveDescribableExp' for the body, and modify it as needed, but
then also write 'referencedTypes'.

@
instance Describable MyType where
  describeType = \$(deriveDescribableExp ''MyType)
  referencedTypes _ = reach \@Inner
@

This requires the module to be compiled with the @-haddock@ GHC flag,
which is set automatically when using this package's build configuration.

__Error__: A compile-time error is raised if no Haddock documentation is found.
-}
deriveDescribable :: Name -> Q [Dec]
deriveDescribable name = do
  body <- deriveDescribableExp name
  (typeVars, fieldTypes) <- getTypeShape name
  let describeTypeClause = ValD (VarP 'describeType) (NormalB body) []
      -- referencedTypes = \_ -> reach @T1 <> (reach @T2 <> (... <> mempty)),
      -- with no list literal, which OverloadedLists would make ambiguous
      referencedTypesBody =
        LamE
          [WildP]
          ( foldr
              (\fieldType rest -> InfixE (Just (AppTypeE (VarE 'reach) fieldType)) (VarE '(<>)) (Just rest))
              (VarE 'mempty)
              fieldTypes
          )
      referencedTypesClause = ValD (VarP 'referencedTypes) (NormalB referencedTypesBody) []
      -- A Type-kinded variable appears bare in the head and needs Describable. Any other variable
      -- is annotated with its kind in the head and needs only Typeable, as do the kind variables
      -- of those kinds (a poly-kinded `a :: k` needs `Typeable k` as well to be Typeable itself).
      headArg (v, Nothing) = VarT v
      headArg (v, Just kind) = SigT (VarT v) kind
      describableVars = [v | (v, Nothing) <- typeVars]
      kindVars =
        nub
          [ kv
          | (_, Just kind) <- typeVars
          , kv <- freeKindVars kind
          , kv `notElem` describableVars
          ]
      instanceHead = foldl AppT (ConT name) (headArg <$> typeVars)
      context =
        concat
          [ case mkind of
              Nothing -> [AppT (ConT ''Describable) (VarT v)]
              Just _ -> [AppT (ConT ''Typeable) (VarT v)]
          | (v, mkind) <- typeVars
          ]
          <> [AppT (ConT ''Typeable) (VarT kv) | kv <- kindVars]
  pure
    [ InstanceD
        Nothing
        context
        (AppT (ConT ''Describable) instanceHead)
        [describeTypeClause, referencedTypesClause]
    ]

-- | The type variables a kind mentions, each once, in order of appearance.
freeKindVars :: Kind -> [Name]
freeKindVars = nub . go
  where
    go (VarT v) = [v]
    go (AppT a b) = go a <> go b
    go (SigT t k) = go t <> go k
    go (ForallT _ _ t) = go t
    go (InfixT a _ b) = go a <> go b
    go _ = []

{- | The type variables of a type, each with its kind unless that is @Type@ ('Nothing'),
and the types of all fields of all its constructors, in declaration order.
-}
getTypeShape :: Name -> Q ([(Name, Maybe Kind)], [Type])
getTypeShape name = do
  info <- reify name
  (binders, cons) <- case info of
    TyConI (DataD _ _ tvs _ cs _) -> pure (tvs, cs)
    TyConI (NewtypeD _ _ tvs _ c _) -> pure (tvs, [c])
    _ -> pure ([], [])
  pure (binderKind <$> binders, concatMap fieldTypesOf cons)
  where
    binderKind (PlainTV v _) = (v, Nothing)
    binderKind (KindedTV v _ k)
      | isTypeKind k = (v, Nothing)
      | otherwise = (v, Just k)

    isTypeKind StarT = True
    isTypeKind (ConT k) = k == ''Kind.Type
    isTypeKind _ = False

    fieldTypesOf (NormalC _ bts) = snd <$> bts
    fieldTypesOf (RecC _ vbts) = [t | (_, _, t) <- vbts]
    fieldTypesOf (InfixC (_, t1) _ (_, t2)) = [t1, t2]
    fieldTypesOf (ForallC _ _ c) = fieldTypesOf c
    fieldTypesOf (GadtC _ bts _) = snd <$> bts
    fieldTypesOf (RecGadtC _ vbts _) = [t | (_, _, t) <- vbts]

{- | Produce the 'describeType' method body for a type, using its Haddock
documentation comment.  Use this inside a manually written
'Describable' instance:

@
instance Describable MyType where
  describeType = \$(deriveDescribableExp ''MyType)
  referencedTypes _ = reach \@Inner
@

__This returns the 'describeType' body only.__ It does not collect the referenced types:
a hand-written instance must write 'referencedTypes' itself, or its field types are not
described to the model. For a wrapper that is @referencedTypes _ = reach \@a@; for a
record, the @reach \@t@ of each of its field types, as 'deriveDescribable' generates.

For a type with several constructors, each constructor is listed as
@- Constructor: doc@ (by name alone if undocumented), followed by its documented
fields indented beneath it.  For a single-constructor type, the constructor's own
doc, if any, is a line of its own right after the type's doc, and the documented
fields are listed directly.  A documented record field is listed as
@- fieldName: doc@, a documented positional field as @- field N: doc@ with @N@ counting
from 1; an undocumented field adds nothing.

Which Haddock layout documents what, for a constructor with positional fields (GHC's
rule for H98 constructors):

@
data Shape
  = -- | Documents the constructor Circle.
    Circle Double
  | Square Double -- ^ Documents the constructor Square, not its argument.
  | -- | Documents the constructor Rect.
    Rect
      Double
      -- ^ Documents the first argument of Rect.
      Double
      -- ^ Documents the second argument of Rect.
@

A one-line trailing comment after the constructor and all its arguments, as on
@Square@, documents the constructor (it renders as @- Square: doc@). A positional
argument's doc is collected only when the constructor has a leading Haddock comment
or each argument stands on a line of its own with its own trailing comment, as on
@Rect@, which renders its two arguments as @- field 1: …@ and @- field 2: …@. An
argument documented in any other way is not collected, because GHC does not attach
the comment to the argument. A leading comment on each argument (a Haddock comment on
the line above the argument's type) is collected the same way; this is the layout
that fourmolu rewrites the trailing comments into, and it renders unchanged.

Prefer 'deriveDescribable' for the common case where a full instance is needed.
-}
deriveDescribableExp :: Name -> Q Exp
deriveDescribableExp name = do
  mdoc <- getDoc (DeclDoc name)
  doc <- case mdoc of
    Nothing ->
      fail $
        "deriveDescribable: No Haddock documentation found for '"
          <> nameBase name
          <> "'.\n"
          <> "Make sure to:\n"
          <> "  1. Add a Haddock comment above the type declaration (-- | ...).\n"
          <> "  2. Compile with the -haddock GHC flag (e.g. add 'ghc-options: -haddock' to your .cabal file).\n"
          <> "  3. Separate the type declaration from this splice with $(pure []) if they are in the same module."
    Just d -> pure d
  constructorDocs <- getConstructorDocs name
  -- Build: \_ -> pack "<doc>"
  -- Haddock docs have a leading space, no trailing newline, and escape "/" as "\/"
  let unescapeHaddock [] = []
      unescapeHaddock ('\\' : '/' : rest) = '/' : unescapeHaddock rest
      unescapeHaddock (c : rest) = c : unescapeHaddock rest
      normalizeDoc = (++ "\n") . dropWhile (== ' ') . unescapeHaddock
      renderField indent (fn, fd) = indent <> "- " <> fn <> ": " <> normalizeDoc fd
      -- A sum lists every constructor (by name alone if undocumented),
      -- with its documented fields indented beneath it.
      renderConstructor (cn, mcd, fields) =
        "- "
          <> cn
          <> maybe "\n" ((": " <>) . normalizeDoc) mcd
          <> concatMap (renderField "  ") fields
      -- A single constructor is the type itself, so its own doc (if any) is a line of its own,
      -- and its fields are listed directly.
      constructorLines = case constructorDocs of
        [(_, mcd, fields)] -> maybe "" normalizeDoc mcd <> concatMap (renderField "") fields
        cons -> concatMap renderConstructor cons
      fullDoc = normalizeDoc doc <> constructorLines
      docLit = litE (stringL fullDoc)
  [|\_ -> pack $docLit|]

{- | Collect, for each constructor of a type, its name, its Haddock (if any), and
the (label, doc) pairs of its documented fields: a record field is labelled by its
name, a positional field (read with @'ArgDoc'@) by @field N@ with @N@ counting from 1.
Undocumented fields are left out.

A one-line trailing comment after a constructor and all its arguments documents the
constructor, not its last argument. GHC attaches argument docs only if the constructor
has a leading Haddock comment, or each argument stands on a line of its own with its
own trailing comment.
-}
getConstructorDocs :: Name -> Q [(String, Maybe String, [(String, String)])]
getConstructorDocs name = do
  info <- reify name
  let cons = case info of
        TyConI (DataD _ _ _ _ cs _) -> cs
        TyConI (NewtypeD _ _ _ _ c _) -> [c]
        _ -> []
  mapM collect cons
  where
    collect con = do
      let cn = conName con
      cdoc <- getDoc (DeclDoc cn)
      recordPairs <- mapM (\fn -> (nameBase fn,) <$> getDoc (DeclDoc fn)) (recFields con)
      positionalPairs <-
        mapM
          (\i -> ("field " <> show (i + 1),) <$> getDoc (ArgDoc cn i))
          [0 .. positionalCount con - 1]
      pure (nameBase cn, cdoc, [(fn, d) | (fn, Just d) <- recordPairs <> positionalPairs])

    conName (NormalC n _) = n
    conName (RecC n _) = n
    conName (InfixC _ n _) = n
    conName (ForallC _ _ c) = conName c
    conName (GadtC (n : _) _ _) = n
    conName (GadtC [] _ _) = mkName ""
    conName (RecGadtC (n : _) _ _) = n
    conName (RecGadtC [] _ _) = mkName ""

    recFields (RecC _ fields) = [fn | (fn, _, _) <- fields]
    recFields (ForallC _ _ c) = recFields c
    recFields (RecGadtC _ fields _) = [fn | (fn, _, _) <- fields]
    recFields _ = []

    -- The number of positional fields; a record constructor has none, its fields are named.
    positionalCount :: Con -> Int
    positionalCount (NormalC _ bts) = length bts
    positionalCount (InfixC {}) = 2
    positionalCount (ForallC _ _ c) = positionalCount c
    positionalCount (GadtC _ bts _) = length bts
    positionalCount (RecC {}) = 0
    positionalCount (RecGadtC {}) = 0
