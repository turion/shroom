{-# LANGUAGE TemplateHaskell #-}

{- | Template Haskell utilities for deriving 'Describe' instances from Haddock
documentation comments.
-}
module Control.Monad.Prompt.TH (deriveDescribeType) where

-- template-haskell
import Language.Haskell.TH

-- text
import Data.Text (pack)

{- | Produce the 'describeType' method body for a type, using its Haddock
documentation comment.  Use this inside a manually written
'Describe' instance so you can also fill in the other methods:

@
-- | A user with a name and an email address.
data User = User { userName :: Text, userEmail :: Text }
-}

{- $(pure [])

instance Describe User where
  describeType = $(deriveDescribeType ''User)
@

This requires the module to be compiled with the @-haddock@ GHC flag,
which is set automatically when using this package's build configuration.

__Error__: If no Haddock documentation is found for the given type, a
compile-time error is raised with a helpful message.
-}

deriveDescribeType :: Name -> Q Exp
deriveDescribeType name = do
  mdoc <- getDoc (DeclDoc name)
  doc <- case mdoc of
    Nothing ->
      fail $
        "deriveDescribeType: No Haddock documentation found for '"
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
