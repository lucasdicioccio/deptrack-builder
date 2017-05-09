{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE RankNTypes #-}

module Recipes
    ( recipes
    , Recipes (..)
    ) where

import Control.Monad
import Data.String.Conversions
import DepTrack
import Devops.Authentication
import Devops.Base
import Devops.BaseImage
import Devops.Binary
import Devops.Callback
import Devops.Cli
import Devops.Debian.User
import Devops.Debian.Base
import Devops.Docker
import Devops.DockerBootstrap
import Devops.Haskell
import Devops.Storage
import Devops.Postgre (libpqDev)

--------------------------------------------------------------------

libsqlite3Dev :: DevOp (DebianPackage "libsqlite3-dev")
libsqlite3Dev = debianPackage

libpcre3Dev :: DevOp (DebianPackage "libpcre3-dev")
libpcre3Dev = debianPackage

--------------------------------------------------------------------

image :: BinaryCall -> DevOp DockerImage
image cb = do
    let base = BaseImageConfig "/sbin/bootstrap-docker-builder" xenial
    let boot = simpleBootstrap "/opt/tmp-docker-builder" base cb
    dockerImage "docker-builder-base-image" boot

--------------------------------------------------------------------

-- | DepTrack as a project.
deptrackProject :: DevOp (StackProject "deptrack")
deptrackProject = fmap fst $ do
    let url = "https://github.com/lucasdicioccio/deptrack-project.git"
    let branch = "master"
    let systemDependencies = return () -- no system deps
    stackProject url branch "deptrack-src" (mereUser "root") `inject` systemDependencies

instance HasBinary (StackProject "deptrack") "deptrack-devops-example-devtools" where

deptrackExample :: DevOp (Binary "deptrack-devops-example-devtools")
deptrackExample = stackInstall "/opt" deptrackProject

--------------------------------------------------------------------

-- | DepTrack-Builder self-hosting as a project.
deptrackBuilderProject :: DevOp (StackProject "deptrack-builder")
deptrackBuilderProject = fmap fst $ do
    let url = "https://github.com/lucasdicioccio/deptrack-builder.git"
    let branch = "master"
    let systemDependencies = return () -- no system deps
    stackProject url branch "deptrack-builder-src" (mereUser "root") `inject` systemDependencies

instance HasBinary (StackProject "deptrack-builder") "deptrack-builder-exe" where

deptrackBuilder :: DevOp (Binary "deptrack-builder-exe")
deptrackBuilder = stackInstall "/opt" deptrackBuilderProject

--------------------------------------------------------------------

-- | Facebook's Duckling project.
ducklingProject :: DevOp (StackProject "duckling")
ducklingProject = fmap fst $ do
    let url = "https://github.com/facebookincubator/duckling.git"
    let branch = "master"
    let systemDependencies = libpcre3Dev
    stackProject url branch "duckling-src" (mereUser "root") `inject` systemDependencies

instance HasBinary (StackProject "duckling") "duckling-example-exe" where

ducklingExample :: DevOp (Binary "duckling-example-exe")
ducklingExample = stackInstall "/opt" ducklingProject

--------------------------------------------------------------------

-- | The great Postgrest tool
postgrestProject :: DevOp (StackProject "postgrest")
postgrestProject = fmap fst $ do
    let url = "https://github.com/begriffs/postgrest.git" 
    let branch = "master"
    let systemDependencies = libpqDev
    stackProject url branch "postgrest-src" (mereUser "root") `inject` systemDependencies

instance HasBinary (StackProject "postgrest") "postgrest" where

postgrestExample :: DevOp (Binary "postgrest")
postgrestExample = stackInstall "/opt" postgrestProject

--------------------------------------------------------------------

data Build = Build { _buildName     :: !Name
                   , _buildTarget   :: !FilePath
                   , _buildArtifact :: !FilePresent
                   }

runDockerBuild :: Continued Build
               -> DevOp DockerImage
               -> DevOp FilePresent
runDockerBuild cont img = do
    let (Build name target _) = eval cont
    let dir = fmap snd (dockerAuth name)
    let copySshKeys = insertDir dir "/root/.ssh"
    let artifact = dockerized (name <> "-build") img cont copySshKeys
    fetchFile target ((fmap . fmap) _buildArtifact artifact)

data BuildType =
    DepTrackProject
  | DepTrackBuilder
  | Duckling
  | Postgrest
  deriving (Read, Show, Enum, Bounded)

pickBuild :: BuildType -> DevOp Build
pickBuild DepTrackProject = _deptrackBuild
pickBuild DepTrackBuilder = _deptrackBuilderBuild
pickBuild Duckling        = _ducklingBuild
pickBuild Postgrest       = _postgrestBuild

_ducklingBuild :: DevOp Build
_ducklingBuild =
    Build "duckling" "/opt/duckling-example-exe" <$> (binaryPresent ducklingExample)

_deptrackBuild :: DevOp Build
_deptrackBuild =
    Build "deptrack" "/opt/deptrack-devops-example-devtools" <$> (binaryPresent deptrackExample)

_deptrackBuilderBuild :: DevOp Build
_deptrackBuilderBuild =
    Build "deptrack-builder" "/opt/deptrack-builder" <$> (binaryPresent deptrackBuilder)

_postgrestBuild :: DevOp Build
_postgrestBuild =
    Build "postgrest" "/opt/postgrest" <$> (binaryPresent postgrestExample)

allBuilds :: [BuildType]
allBuilds = [minBound .. maxBound]

--------------------------------------------------------------------

data Recipes =
    Root
  -- ^ Root of the projects.
  | PrepareSSH
  -- ^ Initializes SSH.
  | DockerChroot
  -- ^ Inside the debootstrap chroot setup (this should be empty and will
  -- likely fail because we don't mount /proc in the chroot).
  | DockerBuild BuildType
  -- ^ Builds a binary inside a Docker.

recipes :: Recipes -> SelfPath -> (Recipes -> Method -> [String]) -> DevOp ()
recipes DockerChroot _ _             = return ()
recipes (DockerBuild x) _ _          = void $ pickBuild x
recipes PrepareSSH _ _               = void $ authorizeBuilderKey
recipes Root self fixCall            = void $ do
    let chrootCb = binaryCall self (fixCall DockerChroot)
    let dockerCb bt = binaryCall self (fixCall (DockerBuild bt))
    let builtBin b = runDockerBuild (continue b pickBuild dockerCb) (image chrootCb)
    traverse builtBin allBuilds

--------------------------------------------------------------------

-- | An SSH-CA.
frechCertificationAuthority :: DevOp SSHCertificateAuthority
frechCertificationAuthority =
    sshCA (sshKeyPair (directory "/opt/ssh-keys") "builder-master-key")

-- | The same SSH-CA as 'frechCertificationAuthority' but when it's been
-- already build with a 'PrepareSSH' call.
--
-- The reason to NOT track this dependency is that you do not always want to
-- destroy the SSHCertificateAuthority on a TurnDown (e.g., because you
-- manually added the key in your .ssh/authorized_keys).
preExistingCertificationAuthority :: DevOp SSHCertificateAuthority
preExistingCertificationAuthority = declare op $
    pure (runDevOp frechCertificationAuthority)
  where
    op = noop "pre-existing-key: builder-master-key" "removed for protecting turndown"

-- | An authorized_keys file you may want to copy to allow Docker Hosts.
authorizeBuilderKey :: DevOp FilePresent
authorizeBuilderKey =
    ioFile "/opt/ssh-keys/authorized_keys"
           (buildAuthorizedKeysContent <$> gitRepoAuthKeys)
  where
    gitRepoAuthKeys = AuthorizedKeys [] <$> pub
    pub = single . publicKey . getCAKeyPair <$> frechCertificationAuthority
    single x = [x]

-- | Directory where Docker host SSH Keys are stored.
dockerKeyDir :: Name -> DevOp DirectoryPresent
dockerKeyDir name =
    directory ("/opt/ssh-keys/build-docker-" <> convertString name)

-- | The key for docker hosts shall be named "id_rsa"
-- for Git+SSH to pick-up painlessly.
dockerKey :: Name -> DevOp SSHSignedUserKey
dockerKey name = signKey preExistingCertificationAuthority $
    sshKeyPair (dockerKeyDir name) "id_rsa"

-- | A copiable .ssh/ directory with signed user keys for docker hosts.
dockerAuth :: Name -> DevOp (SSHSignedUserKey, DirectoryPresent)
dockerAuth name = (,) <$> dockerKey name <*> dockerKeyDir name
