{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Repology where

import Data.List
import Data.Ord
import Data.Text (Text)
import Data.Aeson
import Data.Proxy
import Data.Version
import qualified Network.HTTP.Client.TLS as Http
import Text.ParserCombinators.ReadP
import Servant.Client
import Servant.API

newtype RepoName = RepoName Text
                 deriving stock (Eq, Ord, Show)
                 deriving newtype (FromJSON, ToJSON)

newtype SubRepoName = SubRepoName Text
                    deriving stock (Eq, Ord, Show)
                    deriving newtype (FromJSON, ToJSON)

newtype ProjectName = ProjectName Text
                    deriving stock (Eq, Ord, Show)
                    deriving newtype (FromJSON, ToJSON, ToHttpApiData)

newtype PackageName = PackageName Text
                    deriving stock (Eq, Ord, Show)
                    deriving newtype (FromJSON, ToJSON)

data Package = Package { pkgRepo :: RepoName
                       , pkgSubRepo :: SubRepoName
                       , pkgVersion :: Version
                       , pkgName :: PackageName
                       }
             deriving stock (Show, Ord, Eq)

instance FromJSON Package where
  parseJSON = withObject "package" $ \o ->
    Package <$> o .: "repo"
            <*> o .:? "subrepo" .!= SubRepoName ""
            <*> (parseVersion' =<< (o .: "version"))
            <*> o .: "name"
    where
      parseVersion' s
        | (v, _) : _ <- sortBy (comparing $ length . snd) $ readP_to_S parseVersion s = pure v
        | otherwise = fail $ "failed to parse version: " <> s

type API = "api" 
        :> "v1"
        :> "project"
        :> Capture "project name" ProjectName
        :> Get '[JSON] [Package]

getProject :: ProjectName -> ClientM [Package]
getProject = client (Proxy @API)

defaultClientEnv :: IO ClientEnv
defaultClientEnv = do
  mgr <- Http.newTlsManager
  return $ mkClientEnv mgr (BaseUrl Http "repology.org" 80 "")

