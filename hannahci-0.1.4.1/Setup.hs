module Main where

import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup (InstallFlags)
import System.Process (callCommand)

main :: IO ()
main = defaultMainWithHooks fixpointHooks

fixpointHooks :: UserHooks
fixpointHooks = simpleUserHooks { postInst = setup }

setup :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
setup _ _ desc lbi = do
  putStrLn $ "Post Install: " ++ show binDir
  putStrLn $ "Post Install: " ++ show libDir
  putStrLn $ "Post Install: " ++ show dataDir
  callCommand $ "sudo " ++ (show dataDir) ++ "setup-local.sh"
  return ()
  where binDir  = bindir  allDirs ++ "/"
        libDir  = libdir  allDirs ++ "/"
        dataDir = datadir allDirs ++ "/"
        allDirs = absoluteInstallDirs desc lbi NoCopyDest
