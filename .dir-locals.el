;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

(
 ("backend/src"
  (haskell-mode
   (dante-target . "backend")
   (eval . (setq dante-project-root (directory-file-name (locate-dominating-file buffer-file-name dir-locals-file))))
   (dante-repl-command-line . ("nix-shell" "default.nix" "--run" (concat "cabal new-repl " dante-target " --builddir=dist/ghc/backend")))
   )
  )
 ("backend/test"
  (haskell-mode
   (dante-target . "backend:backend-test")
   (eval . (setq dante-project-root (directory-file-name (locate-dominating-file buffer-file-name dir-locals-file))))
   (dante-repl-command-line . ("nix-shell" "default.nix" "--run" (concat "cabal new-repl " dante-target " --enable-tests" " --builddir=dist/ghc/backend-test")))
  )
 )
 ("backend/bench"
  (haskell-mode
   (dante-target . "backend:backend-bench")
   (eval . (setq dante-project-root (directory-file-name (locate-dominating-file buffer-file-name dir-locals-file))))
   (dante-repl-command-line . ("nix-shell" "default.nix" "--run" (concat "cabal new-repl " dante-target " --enable-benchmarks" " --builddir=dist/ghc/backend-bench")))
   )
  )
 ("common/src"
  (haskell-mode
   (dante-target . "common")
   (eval . (setq dante-project-root (directory-file-name (locate-dominating-file buffer-file-name dir-locals-file))))
   (dante-repl-command-line . ("nix-shell" "default.nix" "--run" (concat "cabal new-repl " dante-target " --builddir=dist/ghc/common")))
   )
  )
 ("common/test"
  (haskell-mode
   (dante-target . "common:common-test")
   (eval . (setq dante-project-root (directory-file-name (locate-dominating-file buffer-file-name dir-locals-file))))
   (dante-repl-command-line . ("nix-shell" "default.nix" "--run" (concat "cabal new-repl " dante-target " --enable-tests" " --builddir=dist/ghc/common-test")))
  )
 )
 ("common/bench"
  (haskell-mode
   (dante-target . "common:common-bench")
   (eval . (setq dante-project-root (directory-file-name (locate-dominating-file buffer-file-name dir-locals-file))))
   (dante-repl-command-line . ("nix-shell" "default.nix" "--run" (concat "cabal new-repl " dante-target " --enable-benchmarks" " --builddir=dist/ghc/common-bench")))
   )
  )
 ("frontend/src"
  (haskell-mode
   (dante-target . "frontend")
   (eval . (setq dante-project-root (directory-file-name (locate-dominating-file buffer-file-name dir-locals-file))))
   (dante-repl-command-line . ("nix-shell" "default.nix" "--run" (concat "cabal new-repl " dante-target " --builddir=dist/ghc/frontend")))
   )
  )
 ("frontend/test"
  (haskell-mode
   (dante-target . "frontend:frontend-test")
   (eval . (setq dante-project-root (directory-file-name (locate-dominating-file buffer-file-name dir-locals-file))))
   (dante-repl-command-line . ("nix-shell" "default.nix" "--run" (concat "cabal new-repl " dante-target " --enable-tests" " --builddir=dist/ghc/frontend-test")))
  )
 )
 ("frontend/bench"
  (haskell-mode
   (dante-target . "frontend:frontend-bench")
   (eval . (setq dante-project-root (directory-file-name (locate-dominating-file buffer-file-name dir-locals-file))))
   (dante-repl-command-line . ("nix-shell" "default.nix" "--run" (concat "cabal new-repl " dante-target " --enable-benchmarks" " --builddir=dist/ghc/frontend-bench")))
   )
  )
)
