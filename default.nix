{ nixpkgs ? import <nixpkgs> {} }:

let pure-platform = import (builtins.fetchTarball https://github.com/grumply/pure-platform/tarball/master) {};

in pure-platform.project ({ pkgs, ... }: {

  minimal = true;

  packages = {
    backend = ./backend;
    shared = ./shared;
    frontend = ./frontend;
  };

  shells = {
    ghc = [ "backend" "shared" "frontend" ];
    ghcjs = [ "shared" "frontend" ];
  };
})
