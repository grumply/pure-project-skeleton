# Multi-package Pure Skeleton with Development Environment

![CI](https://github.com/grumply/pure-project-skeleton/workflows/CI/badge.svg)

This repo is an example of combining `cabal.project`, `Nix`, `node`, `cachix`, and `pure-platform` for an improved development experience.

## First run

Follow the steps at [purehs.cachix.org](https://purehs.cachix.org) to install [nix](https://nixos.org/nix/) and [cachix](https://cachix.org).

## Development

If you're not running an OS and CPU architecture combination for which Cachix has a pre-built and cached copy of Pure's custom `ghcjs-base`, the first build will be very, very slow. Subsequent builds will take advantage of nix memoization. Most recent linux and macos systems will benefit from the binary cache.

### Backend Development

To run a backend development server that will:

- watch backend and shared Haskell and Cabal files for changes
- rebuild and restart the server when necessary

```bash
$ ./ghc npm run dev:backend
```

### Frontend Development

To run a frontend development server that will:

- serve your application at `localhost:8080`
- watch frontend and shared Haskell and Cabal files for changes
- rebuild the application when necessary
- inject newly-built applications into any connected browsers

```bash
$ ./ghcjs npm install
$ ./ghcjs npm run dev:frontend
```

The web server configuration is at `dist/site/bs-config.js`.

### Development Tools

Support is included for both `hie` and `ghcid`.

#### Haskell IDE Engine (hie) with VS Code

[hie](https://github.com/haskell/haskell-ide-engine) is a full-featured Haskell IDE with [hlint](https://github.com/ndmitchell/hlint) integration, quick actions and refactoring, haddock documentation and type on hover, jump to definition, completion, formatting, and more.

* Be sure you followed the steps at [purehs.cachix.org](https://purehs.cachix.org) to enable cached builds as directed in [First run](#first-run)

* Tell `cachix` to use the `all-hies` build cache.
  ```bash
  cachix use all-hies
  ```

* Install the ghc 8.4.4 hie to match pure-platform's ghc version.
  ```bash
  nix-env -iA selection --arg selector 'p: { inherit (p) ghc844; }' -f https://github.com/infinisil/all-hies/tarball/master
  ```

* Install [Haskell Language Server for VS Code](https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server).

* In VS Code settings search for `useHieWrapper` and be sure the `Use Custom Hie Wrapper` checkbox is selected and set the `User Custom Hie Wrapper Path` to
  ```bash
  ${workspaceFolder}/lsp
  ```

#### ghcid

 [ghcid](https://github.com/ndmitchell/ghcid) integration is also available, but has fewer features than `hie`.

* uncomment `pkgs.haskellPackages.ghcid` in the project's `default.nix` tools.
* Run `ghcid` for `frontend`

  ```bash
  ./ghc ghcid -c "cabal new-repl frontend"
  ```

  or for `backend`

  ```bash
  ./ghc ghcid -c "cabal new-repl backend"
  ```

  or for `shared`

  ```bash
  ./ghc ghcid -c "cabal new-repl shared"
  ```

## Production

### `nix-build`

Nix can be used for creating deterministic, production-ready build products. You can use the `nix-build` command to build all or parts of your multi-package project with Nix.

- Build everything

  ```bash
  $ nix-build
    {.. build output omitted ..}
  $ tree result
  result
  ├── ghc
  │   ├── backend -> /nix/store/{..}-backend-0.1.0.0
  │   ├── frontend -> /nix/store/{..}-frontend-0.1.0.0
  │   └── shared -> /nix/store/{..}-shared-0.1.0.0
  └── ghcjs
      ├── frontend -> /nix/store/{..}-frontend-0.1.0.0
      └── shared -> /nix/store/{..}-shared-0.1.0.0

  7 directories, 0 files
  ```

- Build the backend only

  ```bash
  $ nix-build -o backend-result -A ghc.backend
  ```

- Build the frontend only

  ```bash
  $ nix-build -o frontend-result -A ghcjs.frontend
  ```

## Thanks

Thanks to [Will Fancher](https://github.com/elvishjerricco) for [reflex-project-skeleton](https://github.com/elvishjerricco/reflex-project-skeleton) on which this project is based.

