all: build ghc

init:
  git submodule update --init --recursive --remote

test-all:
  test-common
  test-backend
  test-frontend

bench-all:
	bench-common
  bench-backend
	bench-frontend

ghc:
	cabal new-build all --builddir=dist/ghc

ghcjs:
	cabal new-build all --project-file=cabal-ghcjs.project --builddir=dist/ghcjs

test-common:
  cabal new-build common:common-test --builddir=dist/ghc --enable-tests

test-backend:
	cabal new-build backend:backend-test --builddir=dist/ghc --enable-tests

test-frontend:
	cabal new-build frontend:frontend-test --builddir=dist/ghcjs --project-file=cabal-ghcjs.project --enable-tests

bench-common:
  cabal new-build common:common-bench --builddir=dist/ghc --enable-benchmarks

bench-backend:
	cabal new-build backend:backend-bench --builddir=dist/ghc --enable-benchmarks

bench-frontend:
	cabal new-build frontend:frontend-bench --builddir=dist/ghcjs --project-file=cabal-ghcjs.project --enable-benchmarks
