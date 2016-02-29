#!/usr/bin/runghc

\begin{code}
import Data.Maybe(fromMaybe)
import Distribution.PackageDescription(HookedBuildInfo,emptyHookedBuildInfo
                                      ,PackageDescription,emptyBuildInfo
                                      ,BuildInfo(extraLibDirs,includeDirs))
import Distribution.PackageDescription.Parse(writeHookedBuildInfo)
import Distribution.Simple(defaultMainWithHooks,autoconfUserHooks
                          ,preConf,postConf)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup(ConfigFlags(configVerbosity),Flag(Flag))
import Distribution.Verbosity(Verbosity,silent)
import System.Exit(ExitCode(ExitSuccess),exitWith)
import System.Directory(removeFile,findExecutable)
import System.Process(runInteractiveProcess, waitForProcess)
import System.IO(hClose, hGetContents, hPutStr, stderr)
import Control.Monad(when)
import Control.OldException(try)

main = defaultMainWithHooks autoconfUserHooks{preConf= preConf
                                             ,postConf= postConf}
  where
    preConf ::  [String] -> ConfigFlags -> IO HookedBuildInfo
    preConf args flags = do
      try (removeFile "PostgreSQL.buildinfo")
      return emptyHookedBuildInfo
    postConf :: [String] -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
    postConf args flags _ localbuildinfo = do
      mb_bi <- pqConfigBuildInfo (configVerbosity flags)
      writeHookedBuildInfo "PostgreSQL.buildinfo" (Just (fromMaybe emptyBuildInfo mb_bi),[])

\end{code}

The following code is derived from Distribution.Simple.Configure
\begin{code}
findProgram
    :: String              -- ^ program name
    -> Maybe FilePath      -- ^ optional explicit path
    -> IO (Maybe FilePath)
findProgram name Nothing = do
  mb_path <- findExecutable name
  case mb_path of
    Nothing   -> message ("No " ++ name ++ " found")
    Just path -> message ("Using " ++ name ++ ": " ++ path)
  return mb_path
findProgram name (Just path) = do
  message ("Using " ++ name ++ ": " ++ path)
  return (Just path)

rawSystemGrabOutput :: (Flag Verbosity) -> FilePath -> [String] -> IO String
rawSystemGrabOutput verbosity path args = do
  when (verbosity /= Flag silent) $
        putStrLn (path ++ concatMap (' ':) args)
  (inp,out,err,pid) <- runInteractiveProcess path args Nothing Nothing
  exitCode <- waitForProcess pid
  if exitCode /= ExitSuccess
    then do errMsg <- hGetContents err
            hPutStr stderr errMsg
            exitWith exitCode
    else return ()
  hClose inp
  hClose err
  hGetContents out

message :: String -> IO ()
message s = putStrLn $ "configure: " ++ s
\end{code}

Populate BuildInfo using pkg-config tool.
\begin{code}
pqConfigBuildInfo :: (Flag Verbosity) -> IO (Maybe BuildInfo)
pqConfigBuildInfo verbosity = do
  mb_pq_config_path <- findProgram "pg_config" Nothing
  case mb_pq_config_path of
    Just pq_config_path -> do
       message ("configuring pq library") 
       res <- rawSystemGrabOutput verbosity pq_config_path ["--libdir"]
       let lib_dirs = words res
       res <- rawSystemGrabOutput verbosity pq_config_path ["--includedir"]
       let inc_dirs = words res
       res <- rawSystemGrabOutput verbosity pq_config_path ["--includedir-server"]
       let inc_dirs_server = words res
       let bi = emptyBuildInfo{extraLibDirs=lib_dirs, includeDirs=inc_dirs++inc_dirs_server}
       return (Just bi)
    Nothing -> do
       message ("The package will be built using default settings for pq library")
       return Nothing
\end{code}
