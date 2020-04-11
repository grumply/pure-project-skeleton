{ nixpkgs ? import <nixpkgs> {} }:

let pure-platform = import (builtins.fetchTarball https://github.com/grumply/pure-platform/tarball/245affcf90d655ca4333b1e05d3e1ad399758381) {};

in pure-platform.project ({ pkgs, ... }: {

  minimal = true;

  packages = {
    backend = ./backend;
    shared = ./shared;
    frontend = ./frontend;
    server = ./server;
    test = ./test;
    dev = ./dev;
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
