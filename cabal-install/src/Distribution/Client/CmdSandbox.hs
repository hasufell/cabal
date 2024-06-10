{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

-- | cabal-install CLI command: build
module Distribution.Client.CmdSandbox
  ( -- * The @build@ CLI and action
    sandboxCommand
  , sandboxAction
  , updateGlobalFlags
  , isSandbox
  ) where

import Distribution.Utils.Path
import Distribution.Client.Compat.Prelude
import Distribution.Compat.Directory
  ( doesPathExist
  )
import Prelude ()

import Distribution.Client.CmdErrorMessages
import Distribution.Client.CmdSdist
import Distribution.Client.ProjectOrchestration
import Distribution.Client.TargetProblem
  ( TargetProblem (..)
  , TargetProblem'
  )

import Distribution.Client.CmdInstall.ClientInstallFlags
import Distribution.Client.CmdInstall.ClientInstallTargetSelector

import Distribution.Client.Config
  ( SavedConfig (..)
  , defaultInstallPath
  , loadConfig
  )
import Distribution.Client.DistDirLayout
  ( CabalDirLayout (..)
  , DistDirLayout (..)
  , StoreDirLayout (..)
  , cabalStoreDirLayout
  , mkCabalDirLayout
  )
import Distribution.Client.IndexUtils
  ( getInstalledPackages
  , getSourcePackages
  )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallSymlink
  ( Symlink (..)
  , promptRun
  , symlinkBinary
  , symlinkableBinary
  , trySymlink
  )
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ProjectConfig
  ( ProjectPackageLocation (..)
  , fetchAndReadSourcePackages
  , projectConfigWithBuilderRepoContext
  , resolveBuildTimeSettings
  , withGlobalConfig
  , withProjectOrGlobalConfig
  )
import Distribution.Client.ProjectConfig.Types
  ( MapMappend (..)
  , PackageConfig (..)
  , ProjectConfig (..)
  , ProjectConfigBuildOnly (..)
  , ProjectConfigShared (..)
  , getMapLast
  , getMapMappend
  , projectConfigBuildOnly
  , projectConfigConfigFile
  , projectConfigLogsDir
  , projectConfigStoreDir
  )
import Distribution.Client.ProjectFlags (ProjectFlags (..))
import Distribution.Client.ProjectPlanning
  ( storePackageInstallDirs'
  )
import Distribution.Client.ProjectPlanning.Types
  ( ElaboratedInstallPlan
  )
import Distribution.Client.RebuildMonad
  ( runRebuild
  )
import Distribution.Client.Setup
  ( CommonSetupFlags (..)
  , ConfigFlags (..)
  , GlobalFlags (..)
  , InstallFlags (..)
  , reqArgFlag
  , relevantConfigValuesText
  )
import Distribution.Client.Types
  ( PackageLocation (..)
  , PackageSpecifier (..)
  , SourcePackageDb (..)
  , UnresolvedSourcePackage
  , mkNamedPackage
  , pkgSpecifierTarget
  )
import Distribution.Client.Types.OverwritePolicy
  ( OverwritePolicy (..)
  )
import Distribution.Package
  ( Package (..)
  , PackageName
  , mkPackageName
  , unPackageName
  )
import Distribution.Simple.BuildPaths
  ( exeExtension
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  , optionName
  , usageAlternatives
  , option
  )
import Distribution.Simple.Compiler
  ( Compiler (..)
  , CompilerFlavor (..)
  , CompilerId (..)
  , PackageDB (..)
  , PackageDBStack
  )
import Distribution.Simple.Configure
  ( configCompilerEx
  )
import Distribution.Simple.Flag
  ( flagElim
  , flagToMaybe
  , fromFlagOrDefault
  , fromFlag
  , toFlag
  )
import Distribution.Simple.GHC
  ( GhcEnvironmentFileEntry (..)
  , GhcImplInfo (..)
  , ParseErrorExc
  , getGhcAppDir
  , getImplInfo
  , ghcPlatformAndVersionString
  , readGhcEnvironmentFile
  , renderGhcEnvironmentFile
  )
import qualified Distribution.Simple.InstallDirs as InstallDirs
import qualified Distribution.Simple.PackageIndex as PI
import Distribution.Simple.Program.Db
  ( defaultProgramDb
  , prependProgramSearchPath
  , userSpecifyArgss
  , userSpecifyPaths
  )
import Distribution.Simple.Setup
  ( Flag (..)
  , installDirsOptions
  , optionVerbosity
  , globalWorkingDir
  )
import Distribution.Simple.Utils
  ( createDirectoryIfMissingVerbose
  , dieWithException
  , notice
  , ordNub
  , safeHead
  , warn
  , withTempDirectory
  , wrapText
  , die'
  )
import Distribution.Solver.Types.PackageConstraint
  ( PackageProperty (..)
  )
import Distribution.Solver.Types.PackageIndex
  ( lookupPackageName
  , searchByName
  )
import Distribution.Solver.Types.SourcePackage
  ( SourcePackage (..)
  )
import Distribution.System
  ( OS (Windows)
  , Platform
  , buildOS
  )
import Distribution.Types.InstalledPackageInfo
  ( InstalledPackageInfo (..)
  )
import Distribution.Types.PackageId
  ( PackageIdentifier (..)
  )
import Distribution.Types.UnitId
  ( UnitId
  )
import Distribution.Types.UnqualComponentName
  ( UnqualComponentName
  , unUnqualComponentName
  )
import Distribution.Types.Version
  ( Version
  , nullVersion
  )
import Distribution.Types.VersionRange
  ( thisVersion
  )
import Distribution.Utils.Generic
  ( writeFileAtomic
  )
import Distribution.Verbosity
  ( lessVerbose
  , normal
  )

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Ord
  ( Down (..)
  )
import qualified Data.Set as S
import Distribution.Client.Errors
import Distribution.Utils.NubList
  ( fromNubList
  )
import Network.URI (URI)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getTemporaryDirectory
  , makeAbsolute
  , removeDirectory
  , removeFile
  , getCurrentDirectory
  )
import System.FilePath
  ( takeBaseName
  , takeDirectory
  , (<.>)
  , (</>)
  )

data SandboxFlags = SandboxFlags {
  sandboxVerbosity :: Flag Verbosity,
  sandboxLocation  :: Flag FilePath
} deriving Generic

defaultSandboxLocation :: FilePath
defaultSandboxLocation = ".cabal-sandbox"

defaultSandboxFlags :: SandboxFlags
defaultSandboxFlags = SandboxFlags {
  sandboxVerbosity = toFlag normal,
  sandboxLocation  = toFlag defaultSandboxLocation
  }

sandboxCommand :: CommandUI SandboxFlags
sandboxCommand = CommandUI {
  commandName         = "sandbox",
  commandSynopsis     = "Create/modify/delete a sandbox.",
  commandDescription  = Just $ \pname -> wrapText $
         "Sandboxes are isolated nix-style stores that can be used"
      ++ " for directory local workflows to easily create and destroy environments.",

  commandNotes = Just $ \pname ->
       relevantConfigValuesText ["require-sandbox"
                                ,"ignore-sandbox"]
        ++ "\n"
        ++ "Examples:\n"
        ++ "  Set up a sandbox with one local dependency, located at ../foo:\n"
        ++ "    " ++ pname ++ " sandbox init\n"
        ++ "    " ++ pname ++ " install --only-dependencies\n"
        ++ "  Reset the sandbox:\n"
        ++ "    " ++ pname ++ " sandbox delete\n"
        ++ "    " ++ pname ++ " sandbox init\n"
        ++ "    " ++ pname ++ " install --only-dependencies\n",
  commandUsage        = usageAlternatives "v1-sandbox"
    [ "init          [FLAGS]"
    , "delete        [FLAGS]"
    ],

  commandDefaultFlags = defaultSandboxFlags,
  commandOptions      = \_ ->
    [ optionVerbosity sandboxVerbosity
      (\v flags -> flags { sandboxVerbosity = v })

    , option [] ["sandbox"]
      "Sandbox location (default: './.cabal-sandbox')."
      sandboxLocation (\v flags -> flags { sandboxLocation = v })
      (reqArgFlag "DIR")
    ]
  }

sandboxAction :: SandboxFlags -> [String] -> GlobalFlags -> IO ()
sandboxAction sandboxFlags extraArgs globalFlags = do
  let verbosity = fromFlag (sandboxVerbosity sandboxFlags)
  case extraArgs of
    -- Basic sandbox commands.
    ["init"] -> sandboxInit verbosity globalFlags
      -- sandboxInit verbosity sandboxFlags globalFlags
    ["delete"] -> error "delete"

    -- Error handling.
    [] -> die' verbosity $ "Please specify a subcommand (see 'help sandbox')"
    _  -> die' verbosity $ "Unknown 'sandbox' subcommand: " ++ unwords extraArgs


sandboxInit :: Verbosity -> GlobalFlags -> IO ()
sandboxInit verbosity globalFlags = do
  wd <- absoluteWorkingDir Nothing
  let fp = getSandboxFile (Just $ makeSymbolicPath wd)
  b <- doesFileExist fp
  if b
  then warn verbosity ("Sandbox already exists!")
  else writeFile fp ""

sandboxFile :: RelativePath from File
sandboxFile = makeRelativePathEx ".cabal-sandbox.config"

defaultSandboxDir :: RelativePath from (Dir Sandbox)
defaultSandboxDir = makeRelativePathEx ".cabal.sandbox"

getSandboxFile :: Maybe (SymbolicPath CWD (Dir Pkg)) -> FilePath
getSandboxFile dir = interpretSymbolicPath dir sandboxFile

isSandbox :: Maybe (SymbolicPath CWD (Dir Pkg)) -> IO Bool
isSandbox dir = doesFileExist (getSandboxFile dir)

getSandboxStoreDir :: Maybe (SymbolicPath CWD (Dir Pkg)) -> IO (Maybe FilePath)
getSandboxStoreDir dir = do
  let sandboxFile = getSandboxFile dir
  b <- doesFileExist sandboxFile
  if b
  then do
    contents <- stripNewline <$> readFile sandboxFile
    if null contents
    then pure $ Just $ interpretSymbolicPath dir defaultSandboxDir
    else pure (Just contents)
  else pure Nothing

stripNewline :: String -> String
stripNewline = filter (`notElem` "\n\r")

updateGlobalFlags :: Maybe (SymbolicPath CWD (Dir Pkg)) -> GlobalFlags -> IO GlobalFlags
updateGlobalFlags dir globalFlags = do
  getSandboxStoreDir dir >>= \case
    Just storeDir ->
      pure $ globalFlags { globalStoreDir = Flag storeDir }
    Nothing -> pure globalFlags

