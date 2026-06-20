#!/usr/bin/env bash

set -e

install_haskell_toolchain() {
  if which ghcup &>/dev/null; then
    echo "    GHCup is already installed."
    return
  fi

  echo "    Installing GHCup, GHC, cabal, stack, and HLS..."
  BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
  BOOTSTRAP_HASKELL_GHC_VERSION=latest \
  BOOTSTRAP_HASKELL_CABAL_VERSION=latest \
  BOOTSTRAP_HASKELL_INSTALL_STACK=1 \
  BOOTSTRAP_HASKELL_INSTALL_HLS=1 \
  BOOTSTRAP_HASKELL_ADJUST_BASHRC=P \
    sh -s -- -y < <(curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org)

  echo "    Haskell toolchain installed successfully."
}

install_haskell_toolchain
