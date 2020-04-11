module Data.Project (Project(..),project) where

data Project = Frontend | Backend | Shared | Test
  deriving (Eq,Ord,Enum)

project :: Project -> FilePath
project Frontend = "frontend"
project Backend  = "backend"
project Shared   = "shared"
project Test     = "test"
