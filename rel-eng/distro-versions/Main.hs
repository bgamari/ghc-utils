{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import Data.Version

import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Repology
import Servant.Client (runClientM)

data LibcImpl = Glibc | Musl
              deriving (Eq, Ord, Show)

data Distro = Distro { distroName :: Text
                     , distroRepo :: RepoName
                     , distroSubRepo :: SubRepoName
                     , distroLibc :: LibcImpl
                     }
            deriving (Eq, Ord, Show)

relevantProjects :: [ProjectName]
relevantProjects =
  map ProjectName
  [ "glibc"
  , "ncurses"
  , "gmp"
  -- , "binutils"
  -- , "gcc"
  ]

distros :: [Distro]
distros =
  [ fedora "27"
  , fedora "28"
  , fedora "29"
  , fedora "30"

  , centos "7"

  , ubuntu "16.04" "xenial"
  , ubuntu "18.04" "bionic"
  , ubuntu "18.10" "cosmic"
  , ubuntu "19.04" "disco"

  , debian "oldstable"
  , debian "stable"
  , debian "testing"
  , debian "unstable"

  -- , alpine
  -- , freebsd "11"
  ]
  where
    fedora ver =
      Distro { distroName = "Fedora " <> ver
             , distroRepo = RepoName $ "fedora_" <> ver
             , distroSubRepo = SubRepoName "release"
             , distroLibc = Glibc
             }
    centos ver =
      Distro { distroName = "CentOS " <> ver
             , distroRepo = RepoName $ "centos_" <> ver
             , distroSubRepo = SubRepoName "os"
             , distroLibc = Glibc
             }
    ubuntu ver code =
      Distro { distroName = "Ubuntu " <> ver <> " (" <> code <> ")"
             , distroRepo = RepoName $ "ubuntu_" <> T.map f ver
             , distroSubRepo = SubRepoName $ code <> "/main"
             , distroLibc = Glibc
             }
      where f '.' = '_'
            f x   = x
    debian ver =
      Distro { distroName = "Debian " <> ver
             , distroRepo = RepoName $ "debian_" <> ver
             , distroSubRepo = SubRepoName $ ver <> "/main"
             , distroLibc = Glibc
             }

truncateVersion :: Int -> Version -> Version
truncateVersion n (Version xs _) = Version (take n xs) []
--truncateVersion n = id

main :: IO ()
main = do
  clientEnv <- defaultClientEnv
  projectPackages <- runClientM (mapM getProject $ M.fromList [(p,p) | p <- relevantProjects]) clientEnv >>= either (fail . show) return
  --print projectPackages

  let repoToDistro :: M.Map (RepoName, SubRepoName) Distro
      repoToDistro =
        M.fromList
        [ ((distroRepo d, distroSubRepo d), d)
        | d <- distros
        ]

  let distroPackages :: M.Map Distro (M.Map ProjectName Version)
      distroPackages =
        M.fromListWith (<>)
        [ (distro, M.singleton projName (pkgVersion pkg))
        | (projName, pkgs) <- M.toList projectPackages
        , pkg <- fmap (\pkg -> pkg { pkgVersion = truncateVersion 2 $ pkgVersion pkg }) pkgs
        , Just distro <- pure $ M.lookup (pkgRepo pkg, pkgSubRepo pkg) repoToDistro
        ]
  --print distroPackages

  let combinations :: M.Map (M.Map ProjectName Version) (S.Set Distro)
      combinations =
        M.fromListWith (<>)
        [ (x, S.singleton y) | (y, x) <- M.toList distroPackages ]
  mapM_ putStrLn $ 
    [ unwords [ T.unpack proj <> "-" <> showVersion ver
              | (ProjectName proj, ver) <- M.toList pkgs
              ]
      <> "\t" <> show (fmap distroName $ S.toList distros)
    | (pkgs, distros) <- M.toList combinations
    ]
