{ nixpkgs ? import <nixpkgs> {} }:

let pure-platform = import (builtins.fetchTarball https://github.com/grumply/pure-platform/tarball/90e69c3de91893df149c702b006c6e48d4b3eb8c) {};

in pure-platform.project ({ pkgs, ... }: {

  minimal = true;

  packages = {
    backend = ./app/backend;
    shared = ./app/shared;
    frontend = ./app/frontend;

    server = ./dev/server;
    dev = ./dev/dev;

  };

  overrides = self: super: {
  };

  shells = {
    ghc = [ "dev" "server" "backend" "shared" "frontend" ];
    ghcjs = [ "shared" "frontend" ];
  };

  tools = ghc: with ghc; [
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.fsnotify
    pkgs.pkgs.dhall-json
  ];

})
