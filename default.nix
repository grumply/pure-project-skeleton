{}:

(import ./deps/pure-platform {}).project ({ pkgs, ... }: {
  overrides = self: super: {

  };

  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
})
