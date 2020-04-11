module Data.Opts where

import Data.Compiler

import Options.Applicative

newtype Opts = Opts { compiler :: Compiler }

getOpts :: IO Opts
getOpts = execParser $ info ((Opts <$> compiler) <**> helper)
  ( fullDesc
  <> progDesc "Continuously build for development and testing."
  <> header "dev - a continous build system"
  )
  where
    compiler :: Parser Compiler
    compiler = ghc <|> ghcjs
      where
        ghc   = flag' GHC   ( long "ghc"   <> help "Continuously build for GHC"   )
        ghcjs = flag' GHCJS ( long "ghcjs" <> help "Continuously build for GHCJS" )

