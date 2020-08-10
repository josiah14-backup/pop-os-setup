#!/usr/bin/env bash


if ! which stack ; then
  sudo apt install -y haskell-stack
  stack upgrade --binary-only
else
  echo "    ...is already installed."
fi
