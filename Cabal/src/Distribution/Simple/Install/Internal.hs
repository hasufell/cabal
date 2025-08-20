{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

module Distribution.Simple.Install.Internal where

import Distribution.Compat.Prelude
import Data.Either (rights)
import Prelude ()


import Distribution.Types.LocalBuildInfo
import Distribution.Simple.Setup.Config (ConfigFlags(..))
import Distribution.PackageDescription
import Distribution.Simple.Utils
  ( createDirectoryIfMissingVerbose
  , dieWithException
  , installOrdinaryFile
  )
import Distribution.Utils.Path

import Distribution.Simple.Errors

import System.Directory
  ( doesFileExist
  )
import System.FilePath
  ( takeDirectory
  )

import Distribution.Verbosity

-- | Install the files listed in install-includes for a library
--
-- We only gather the files from the build output directory, not the source directory.
installIncludeFiles :: Verbosity -> BuildInfo -> LocalBuildInfo -> FilePath -> FilePath -> IO ()
installIncludeFiles verbosity libBi _lbi buildPref destIncludeDir = do
  let relincdirs = sameDirectory : mapMaybe symbolicPathRelative_maybe (includeDirs libBi)
      incdirs =
        [ (root, getSymbolicPath dir)
        | dir <- relincdirs
        , root <- [buildPref]
        ]
  incs <- traverse (\inc -> either (dieWithException verbosity) pure =<< (findInc incdirs $ getSymbolicPath inc)) (installIncludes libBi)
  sequence_
    [ do
      createDirectoryIfMissingVerbose verbosity True destDir
      installOrdinaryFile verbosity srcFile destFile
    | (relFile, _, srcFile) <- incs
    , let destFile = destIncludeDir </> relFile
          destDir = takeDirectory destFile
    ]

-- | "Move" the 'install-includes' to the build output directory after a successful build, so
-- they're in the same plase as 'autogen-includes.
--
-- Since 'autogen-includes' can be listed in 'install-includes', we have to ignore cases where
-- we don't find files in the source dir. We could just take @installIncludes \\ autogenIncludes@,
-- but there seems to be some instability across cabal file versions regarding how both
-- fields are accumulated.
-- This also sadly means that we don't have a whole lot of error handling here, as
-- we silently ignore missing files. But the installation proper will uncover such.
syncIncludeFiles :: Verbosity -> BuildInfo -> LocalBuildInfo -> FilePath -> IO ()
syncIncludeFiles verbosity libBi lbi buildPref = do
  let relincdirs = sameDirectory : mapMaybe symbolicPathRelative_maybe (includeDirs libBi)
      incdirs =
        [ (root , getSymbolicPath dir)
        | dir <- relincdirs
        , root <- [baseDir lbi]
        ]
  incs <- rights <$> traverse (findInc incdirs . getSymbolicPath) (installIncludes libBi)
  sequence_
    [ do
      createDirectoryIfMissingVerbose verbosity True destDir
      installOrdinaryFile verbosity srcFile destFile
    | (relFile, subdir, srcFile) <- incs
    , let destFile = buildPref </> subdir </> relFile
          destDir = takeDirectory destFile
    ]

baseDir :: LocalBuildInfo -> FilePath
baseDir lbi = packageRoot $ configCommonFlags $ configFlags lbi

findInc :: [(FilePath, FilePath)] -> String -> IO (Either CabalException (String, FilePath, FilePath))
findInc [] file = pure $ Left (CantFindIncludeFile file)
findInc ((bDir, dir) : dirs) file = do
  let path = bDir </> dir </> file
  exists <- doesFileExist path
  if exists then return (Right (file, dir, path)) else findInc dirs file

