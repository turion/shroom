{-# LANGUAGE TemplateHaskell #-}

{- | Template Haskell utilities for deriving 'Describable' instances from Haddock
documentation comments.
-}
module Control.Monad.Prompt.TH (deriveDescribable, deriveDescribableExp) where

-- template-haskell
import Language.Haskell.TH

-- text
import Data.Text (pack)

-- shroom
import Data.Shroom.Class (Describable (describeType))

{- | Generate a complete 'Describable' instance for a type, deriving
'describeType' from its Haddock documentation comment.  Use this when
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
@

To override 'describeType' manually, write the 'Describable' instance by hand.
You can use 'deriveDescribableExp' for the body, and modify it as needed.

@
instance Describable MyType where
  describeType = \$(deriveDescribableExp ''MyType)
@

This requires the module to be compiled with the @-haddock@ GHC flag,
which is set automatically when using this package's build configuration.

__Error__: A compile-time error is raised if no Haddock documentation is found.
-}
deriveDescribable :: Name -> Q [Dec]
deriveDescribable name = do
  body <- deriveDescribableExp name
  let describeTypeClause = ValD (VarP 'describeType) (NormalB body) []
  pure
    [ InstanceD
        Nothing
        []
        (AppT (ConT ''Describable) (ConT name))
        [describeTypeClause]
    ]

{- | Produce the 'describeType' method body for a type, using its Haddock
documentation comment.  Use this inside a manually written
'Describable' instance:

@
instance Describable MyType where
  describeType = \$(deriveDescribableExp ''MyType)
@

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
  fieldDocs <- getRecordFieldDocs name
  -- Build: \_ -> pack "<doc>"
  -- Haddock docs have a leading space, no trailing newline, and escape "/" as "\/"
  let unescapeHaddock [] = []
      unescapeHaddock ('\\' : '/' : rest) = '/' : unescapeHaddock rest
      unescapeHaddock (c : rest) = c : unescapeHaddock rest
      normalizeDoc = (++ "\n") . dropWhile (== ' ') . unescapeHaddock
      fieldLines = concatMap (\(fn, fd) -> "- " <> fn <> ": " <> normalizeDoc fd) fieldDocs
      fullDoc = normalizeDoc doc <> fieldLines
      docLit = litE (stringL fullDoc)
  [|\_ -> pack $docLit|]

-- | Collect (fieldName, doc) pairs for all record fields of a type.
getRecordFieldDocs :: Name -> Q [(String, String)]
getRecordFieldDocs name = do
  info <- reify name
  let fieldNames = case info of
        TyConI (DataD _ _ _ _ cons _) -> concatMap recFields cons
        TyConI (NewtypeD _ _ _ _ con _) -> recFields con
        _ -> []
  pairs <- mapM (\fn -> (nameBase fn,) <$> getDoc (DeclDoc fn)) fieldNames
  pure [(fn, d) | (fn, Just d) <- pairs]
  where
    recFields (RecC _ fields) = [fn | (fn, _, _) <- fields]
    recFields _ = []
