#!/usr/bin/env bash
set -e

# Install ghcup if it isn't present
if ! command -v ghcup >/dev/null 2>&1; then
    echo "Installing ghcup..."
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
      | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh
fi

# Ensure the ghcup binaries are on PATH
export PATH="$HOME/.ghcup/bin:$PATH"

# Install latest GHC, Cabal, and Stack
ghcup install ghc latest
ghcup install cabal latest
ghcup install stack latest

# Set the installed versions as default
ghcup set ghc latest
ghcup set cabal latest

# Add ghcup to bashrc if not already
if ! grep -q 'ghcup/bin' "$HOME/.bashrc" 2>/dev/null; then
  echo 'export PATH="$HOME/.ghcup/bin:$PATH"' >> "$HOME/.bashrc"
fi

echo "Haskell toolchain installed. Open a new shell or source ~/.bashrc to use ghc, cabal, and stack."
