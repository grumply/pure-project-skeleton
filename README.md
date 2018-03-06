Multi-package Pure example
---

This repo is an example of combining `cabal.project`, Nix, node, and
`pure-platform` to improve the developer experience.

First run
---

Either clone this repo with `--recurse-submodules`, or run `git
submodule update --init --recursive` in this directory after cloning
to make sure `pure-platform` is checked out. 

First, run `./deps/pure-platform/try-pure` at least once. We won't 
manually use it at all in this project, but it does some extra work to 
setup your system requirements automatically, namely installing Nix and
configuring the Pure binary cache.

Once Nix is installed, everything else is mostly handled for you. To build
everything for the first time, using node:

```bash
$ npm install
$ npm run init
```

Development
---

While developing, have the node dev script running to manage re-builds and serve
the javascript benchmarks, tests, and application.

```bash
$ npm run dev
```

Changes to `backend/bench/*` will cause a recompilation of backend:backend-bench
with GHC followed by automatically running the benchmarks. Similarly for 
`common/bench/*`. However, changes to `frontend/bench/*` will cause a 
recompilation of frontend:frontend-bench with GHCJS followed by reloading the 
browser tab running the benchmarks. The same applies for tests.

Changes to `backend/src/*` will cause a recompilation of backend with GHC 
followed by restarting the server.

Under the hood, node will use Nix and new-cabal to build all of the executables 
in a contained fashion to minimize and time for development builds.

A `.dir-locals.el` is included to allow for emacs+dante development.

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

How it works
---

See
[project-development.md](https://github.com/grumply/pure-platform/blob/master/docs/project-development.md) where
local and non-hackage dependency management are also covered.

Thanks
---

Thanks to [Will Fancher](https://github.com/elvishjerricco) for 
[reflex-project-skeleton](https://github.com/elvishjerricco/reflex-project-skeleton) 
which this project is based on.

TODO:
---

Demonstrate Trivial benchmarking and testing.
