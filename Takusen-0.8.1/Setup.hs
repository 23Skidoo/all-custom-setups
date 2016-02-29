-- #!/usr/bin/env runhaskell 

{-# OPTIONS -cpp #-}
{-# LANGUAGE CPP #-}

-- Can't use "#!" with -cpp; get "invalid preprocessing directive #!"

import Control.Exception (bracket)
import Control.Monad (when)
import Data.List (isPrefixOf, unionBy)
import Distribution.PackageDescription
  ( PackageDescription(..), Library(..), BuildInfo(..), HookedBuildInfo
  , emptyHookedBuildInfo, writeHookedBuildInfo, emptyBuildInfo, hasLibs
  )
import Distribution.Simple.Setup -- ( --Flag, fromFlag, toFlag
  ( ConfigFlags(..), BuildFlags(..)
  , InstallFlags(..), HaddockFlags(..)
  , CopyFlags(..)
  , RegisterFlags(..), emptyRegisterFlags
  , CopyDest(..)
  )
import Distribution.Simple
  ( defaultMainWithHooks, defaultUserHooks, UserHooks(..), Args )
import Distribution.Simple.Build (build)
import Distribution.Simple.Install (install)
import Distribution.Simple.Program (findProgramOnPath)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.PreProcess (PPSuffixHandler, knownSuffixHandlers)
import Distribution.Simple.Register
  (register, unregister, writeInstalledConfig,removeRegScripts)
import Distribution.Verbosity (silent, normal)
import System.Directory (getTemporaryDirectory, canonicalizePath, removeFile)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (splitFileName, combine)
import System.IO (hClose, openFile, hGetLine, hIsEOF, openTempFile, IOMode(..))
import System.IO.Error (try, ioError)
import System.Process(runProcess, waitForProcess)

{-
One install script to rule them all, and in the darkness build them...

Some of the code in this script is adapted from the various
HSQL Setup scripts, so some credit for it should go to Krasimir Angelov.
Not sure exactly what that means for our license;
does he have to appear in our license.txt?
(His code is also BSD3 licensed.)

See this page for useful notes on tagging and releasing:
  http://www.haskell.org/haskellwiki/How_to_write_a_Haskell_program

To-dos for Takusen:
 - use hsc2hs to create #define constants from header files,
   rather than hard-code them.
 - Blob support (and clob?).
 - ODBC back-end.
 - FreeTDS back-end.
 - POP3 & IMAP back-ends?

 - Unwritten tests:
   * incorrect fold function (doesn't match result-set)


GHC compiler/linker options:

ODBC    : <none on Windows, because the .dll is in c:\windows\system32, and the headers are in ghc's include/mingw>
Postgres: -I"C:\Program Files\PostgreSQL\8.1\include" -lpq -L"C:\Program Files\PostgreSQL\8.1\bin"
Sqlite  : -I"C:\Program Files\sqlite" -lsqlite3 -L"C:\Program Files\sqlite"
Oracle  : -I"C:\Program Files\Oracle\OraHome817\oci\include" -loci -L"C:\Program Files\Oracle\OraHome817\bin"
Oracle  : -I"%ORACLE_HOME%\oci\include" -loci -L"%ORACLE_HOME%\bin"
-}

main = defaultMainWithHooks defaultUserHooks
  { preConf=preConf, postConf=postConf
  , buildHook=buildHook, instHook=installHook
  }
  where
    preConf :: Args -> ConfigFlags -> IO HookedBuildInfo
    preConf args flags = do
      try (removeFile "takusen.buildinfo")
      return emptyHookedBuildInfo
    postConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
    postConf args flags _ localbuildinfo = do
      let verbose = (configVerbose flags)
      sqliteBI <- configSqlite3 verbose
      pgBI <- configPG verbose
      oraBI <- configOracle verbose
      odbcBI <- configOdbc verbose
      let bis = [sqliteBI, pgBI, oraBI, odbcBI]
      writeHookedBuildInfo "takusen.buildinfo" (concatBuildInfo bis,[])
    -- We patch in the buildHook so that we can modify the list of exposed
    -- modules (we remove modules for back-ends that are not installed).
    buildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
    buildHook pd lbi uh bf = defaultBuildHook (modifyPackageDesc pd) lbi uh bf
    -- also patch the installHook
    installHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
    installHook pd lbi uh insf = defaultInstallHook (modifyPackageDesc pd) lbi uh insf

-- BEGIN: Copied verbatim from Distribution.Simple (in Cabal-1.2.2.0)
defaultInstallHook :: PackageDescription -> LocalBuildInfo
                   -> UserHooks -> InstallFlags -> IO ()
defaultInstallHook pkg_descr localbuildinfo _ (InstallFlags uInstFlag verbosity) = do
  install pkg_descr localbuildinfo (CopyFlags NoCopyDest verbosity)
  when (hasLibs pkg_descr) $
      register pkg_descr localbuildinfo
           emptyRegisterFlags{ regPackageDB=uInstFlag, regVerbose=verbosity }

defaultBuildHook :: PackageDescription -> LocalBuildInfo
	-> UserHooks -> BuildFlags -> IO ()
defaultBuildHook pkg_descr localbuildinfo hooks flags = do
  build pkg_descr localbuildinfo flags (allSuffixHandlers hooks)
  when (hasLibs pkg_descr) $
      writeInstalledConfig pkg_descr localbuildinfo False Nothing

allSuffixHandlers :: UserHooks
                  -> [PPSuffixHandler]
allSuffixHandlers hooks
    = overridesPP (hookedPreProcessors hooks) knownSuffixHandlers
    where
      overridesPP :: [PPSuffixHandler] -> [PPSuffixHandler] -> [PPSuffixHandler]
      overridesPP = unionBy (\x y -> fst x == fst y)

-- END: Copied verbatim from Distribution.Simple


modifyPackageDesc pd =
  let
    Just (Library modules buildInf) = library pd
    filteredMods = filterModulesByLibs modules (extraLibs buildInf)
  in pd { library = Just (Library filteredMods buildInf) }

filterModulesByLibs modules libs =
  removeModulesForAbsentLib "pq" "Database.PostgreSQL" libs
  . removeModulesForAbsentLib "oci" "Database.Oracle" libs
  . removeModulesForAbsentLib "sqlite3" "Database.Sqlite" libs
  . filterODBCModules libs
  $ modules

removeModulesForAbsentLib lib prefix libs modules =
  if not (elem lib libs)
  then filter (not . isPrefixOf prefix) modules
  else modules

filterODBCModules libs modules =
  if not (elem "odbc" libs || elem "odbc32" libs)
  then filter (not . isPrefixOf "Database.ODBC") modules
  else modules


sameFolder path = return (fst (splitFileName path))
parentFolder path = canonicalizePath (fst (splitFileName path) ++ "/..")

makeConfig path libName libDir includeDir = do
  libDirs <- canonicalizePath (combine path libDir)
  includeDirs <- canonicalizePath (combine path includeDir)
  return (Just emptyBuildInfo
        { extraLibs = [libName]
        , extraLibDirs = [libDirs]
        , includeDirs = [includeDirs]
        }
        )

message s = putStrLn ("configure: takusen: " ++ s)

findProgram exe _ = findProgramOnPath exe normal

createConfigByFindingExe desc exe relativeFolder libName libDir includeDir = do
  mb_location <- findProgram exe Nothing
  case mb_location of
    Nothing -> message ("No " ++ desc ++ " (" ++ libName ++ ") library found") >> return Nothing
    Just location -> do
      path <- relativeFolder location
      message ("Using " ++ libName ++ ": " ++ path)
      makeConfig path libName libDir includeDir


configSqlite3 verbose = createConfigByFindingExe "Sqlite" "sqlite3" sameFolder "sqlite3" "" ""

configOracle verbose = createConfigByFindingExe "Oracle" "sqlplus" parentFolder "oci" "bin" "oci/include"

-- On Windows the ODBC stuff is in c:\windows\system32, which is always in the PATH.
-- So I think we only need to pass -lodbc32.
-- The include files are already in the ghc/include/mingw folder.
-- FIXME: I don't know how this should look for unixODBC.

#if mingw32_HOST_OS || mingw32_TARGET_OS
configOdbc verbose = do
  message ("Using odbc: <on Windows => lib already in PATH>")
  return ( Just emptyBuildInfo { extraLibs = ["odbc32"] })
#else
configOdbc verbose = do
  --message ("Using odbc: <on *nix => assume lib already in PATH>")
  --return ( Just emptyBuildInfo { extraLibs = ["odbc"] })
  return ( Just emptyBuildInfo )
#endif

--configPG :: Int -> IO (Maybe BuildInfo)
configPG verbose = do
  mb_pq_config_path <- findProgram "pg_config" Nothing
  case mb_pq_config_path of
    Nothing -> message ("No PostgreSQL (pq) library found") >> return Nothing
    Just pq_config_path -> do
      message ("Using pq: " ++ pq_config_path)
      res <- rawSystemGrabOutput verbose pq_config_path ["--libdir"]
      let lib_dirs = words res
      res <- rawSystemGrabOutput verbose pq_config_path ["--includedir"]
      let inc_dirs = words res
      res <- rawSystemGrabOutput verbose pq_config_path ["--includedir-server"]
      let inc_dirs_server = words res
      return ( Just emptyBuildInfo
        { extraLibs = ["pq"]
        , extraLibDirs = lib_dirs
        , includeDirs = inc_dirs ++ inc_dirs_server
        })

concatBuildInfo = foldr1 combineBuildInfo 

combineBuildInfo mbi Nothing = mbi
combineBuildInfo Nothing mbi = mbi
combineBuildInfo (Just bi1) (Just bi2) =
  Just bi1
  { extraLibs = extraLibs bi1 ++ extraLibs bi2
  , extraLibDirs = extraLibDirs bi1 ++ extraLibDirs bi2
  , includeDirs = includeDirs bi1 ++ includeDirs bi2
  }

--rawSystemGrabOutput :: Int -> FilePath -> [String] -> IO String
rawSystemGrabOutput verbose path args = do
  when (verbose > normal) . putStrLn . unwords $ path:args
  tmp_dir <- getTemporaryDirectory
  (outf,outh) <- openTempFile tmp_dir "out.dat"
  -- process' stderr goes to our stderr
  pid <- runProcess path args Nothing Nothing Nothing (Just outh) Nothing
  exitCode <- waitForProcess pid
  when (exitCode /= ExitSuccess) $ exitWith exitCode
  file_to_contents outf

-- properly, this time, with no lazy IO
file_to_contents fname = 
   bracket (openFile fname ReadMode) 
       (\h -> hClose h >> removeFile fname) 
       (reader [])
 where
 reader acc h = do
        eof <- hIsEOF h
        if eof then return . concat $ reverse acc
           else do
            l <- hGetLine h
            reader (" ":l:acc) h
