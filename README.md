# Multi-package Pure Skeleton with Development Environment

[![Build Status](https://travis-ci.org/grumply/pure-project-skeleton.svg?branch=master)](https://travis-ci.org/grumply/pure-project-skeleton)

This repo is an example of combining `cabal.project`, `Nix`, `node`, `cachix`, and `pure-platform` for an improved development experience.

## First run

Follow the steps at [purehs.cachix.org](https://purehs.cachix.org) to install [nix](https://nixos.org/nix/) and [cachix](https://cachix.org).

## Development

> If you're not running an OS and CPU architecture combination for which Cachix has a pre-built and cached copy of Pure's custom `ghcjs-base`, the first run built will be very, very slow. Subsequent builds will take advantage of nix memoization.

> These two development servers avoid the need to re-enter a nix shell for every build. This can greatly speed up build times compared to `nix-build`.

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

To install [ghcid](https://github.com/ndmitchell/ghcid):

```bash
nix-env -iA nixpkgs.haskellPackages.ghcid
```

To run `ghcid` for `frontend`:

```bash
./ghc ghcid -c "cabal new-repl frontend"
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

