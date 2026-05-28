# Docs

- No local dependency doc lookups (not in /nix/store, ~/.cabal)
- Lookup docs on Hoogle or Hackage
- Hoogle JSON API:
  ```
  curl -s 'https://hoogle.haskell.org/' \
    -G --data-urlencode 'hoogle=FUNCTION_NAME' --data-urlencode 'mode=json' \
    | sed 's/<[^>]*>//g' | grep -o '"item":"[^"]*"'
  ```
