Multi-package Pure example
---

This repo is an example of combining `cabal.project`, Nix, and
`pure-platform` to improve the developer experience.

Either clone this repo with `--recurse-submodules`, or run `git
submodule update --init --recursive` in this directory after cloning
to make sure `pure-platform` is checked out.

First, run `./pure-platform/try-pure` at least once. We won't use
it at all in this project, but it does some extra work to setup your
system requirements automatically, namely installing Nix and
configuring the Pure binary cache.

Once Nix is installed, everything else is mostly handled for you. To
build the project's backend, use the `./cabal` script:

```bash
$ ./cabal new-build all
```

To build the GHCJS frontend app, use the `./cabal-ghcjs` script:

```bash
$ ./cabal-ghcjs new-build all
```

`nix-build`
---

Nix is useful for creating deterministic, production ready build
products. You can use the `nix-build` command to build all the parts
of the project with Nix.

- Build everything

  ```bash
  $ nix-build
  trace:

  Skipping ios apps; system is x86_64-linux, but x86_64-darwin is needed.
  Use `nix-build -A all` to build with remote machines.
  See: https://nixos.org/nixos/manual/options.html#opt-nix.buildMachines


  /nix/store/{..}-pure-project

  $ tree result
  result
  ├── ghc
  │   ├── backend -> /nix/store/{..}-backend-0.1.0.0
  │   ├── common -> /nix/store/{..}-common-0.1.0.0
  │   └── frontend -> /nix/store/{..}-frontend-0.1.0.0
  └── ghcjs
      ├── common -> /nix/store/{..}-common-0.1.0.0
      └── frontend -> /nix/store/{..}-frontend-0.1.0.0

  9 directories, 0 files
  ```

- Build the backend

  ```bash
  $ nix-build -o backend-result -A ghc.backend
  ```

- Build the frontend

  ```bash
  $ nix-build -o frontend-result -A ghcjs.frontend
  ```

Motivation
---

Building a multi-package project with Nix can be a pain because of
Nix's lack of incremental building. A small change to a common package
will require Nix to rebuild that package from scratch, causing a huge
interruption during development. Although this is usually where Stack
would shine, Stack doesn't officially support using Nix for Haskell
derivations, and has zero support for Nix with GHCJS. You *can* build
Pure apps using only Stack and no Nix, but you lose a lot of
benefits that `pure-platform` provides, like the curated set of
package versions that Pure works best with and binary caches of all 
the Haskell derivations.

How it works
---

See
[project-development.md](https://github.com/grumply/pure-platform/blob/master/docs/project-development.md).
