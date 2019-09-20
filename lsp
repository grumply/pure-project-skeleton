#!/usr/bin/env bash

nix-shell default.nix -A shells.ghc --run "hie --lsp"
