{ nixpkgs ? import <nixpkgs> {} }:

let pure-platform = import (builtins.fetchTarball https://github.com/grumply/pure-platform/tarball/6fd7616e8ac4c34bbc78fac3fc3ec3209c563861) {};

in pure-platform.project ({ pkgs, ... }: {

  minimal = false;

  packages = {
    backend = ./app/backend;
    shared = ./app/shared;
    frontend = ./app/frontend;
    test = ./app/test;

    server = ./dev/server;
    dev = ./dev/dev;
  };

  shells = {
    ghc = [ "dev" "server" "backend" "shared" "frontend" "test" ];
    ghcjs = [ "shared" "frontend" ];
  };

  tools = ghc: with ghc; [
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.fsnotify
    pkgs.pkgs.dhall-json
  ];

})
