#! /usr/bin/env bash

# Builds the Haddock documentation as CI does. Fails on any Haddock or compiler
# warning the docs build prints, such as an identifier that is out of scope, as
# the build is meant to be warning-free. `cabal haddock` exits with zero on such
# a warning, so this script scans its output. Stays quiet on success. Arguments
# go on to `cabal haddock`.

# No `set -e`: the script captures cabal's exit status itself, to print the
# output before it exits.
set -u

status=0
output=$(cabal haddock all -f-anthropic -f-ollama \
    --haddock-hyperlink-source --haddock-quickjump \
    --haddock-html-location='https://hackage.haskell.org/package/$pkg-$version/docs' \
    "$@" 2>&1) || status=$?

if [ "$status" -ne 0 ]
then
    printf '%s\n' "$output"
    echo "haddock_check.sh: cabal haddock failed with exit code $status" >&2
    exit "$status"
fi

# Two notices concern the environment, not the docs, so they are not warnings
# to fail on: Haddock's one about dependencies without installed documentation,
# and cabal's one about an out-of-date package list. The match takes `Warning:`
# at the start of a line, as Haddock prints it, or after a non-alphanumeric
# character, as in GHC's `src/Foo.hs:3:1: warning:`, but not the end of a word.
warnings=$(printf '%s\n' "$output" \
    | grep -E '(^|[^[:alnum:]])[Ww]arning:' \
    | grep -v -e '^Warning: The following packages have no Haddock documentation installed' \
              -e "^Warning: The package list for '[^']*' is [0-9]* days old")

if [ -n "$warnings" ]
then
    printf '%s\n' "$output"
    echo "haddock_check.sh: the documentation has warnings:" >&2
    printf '%s\n' "$warnings" >&2
    exit 1
fi
