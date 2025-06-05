#!/bin/sh
set -e

if command -v stack >/dev/null 2>&1; then
  stack test
elif command -v cabal >/dev/null 2>&1; then
  cabal test
else
  echo "Haskell toolchain not available, skipping tests"
fi
