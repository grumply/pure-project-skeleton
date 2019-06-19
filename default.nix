{ nixpkgs ? import <nixpkgs> {} }:

let inherit (nixpkgs) fetchFromGitHub;
    pure-platform = import (fetchFromGitHub {
      owner = "grumply";
      repo = "pure-platform";
      rev = "0cb3a8ba6fa0cb01b2c8958f9ba051bbef4065df";
      sha256 = "1qw6yrc22kmzccnxzh0n40x3y3jw9sx79ygwwg3s3l3hgyljn5ks";
    }) {};

in pure-platform.project ({ pkgs, ... }: {
  
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
