{}:
(import ./deps/pure-platform {}).project ({ pkgs, ... }: {
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
