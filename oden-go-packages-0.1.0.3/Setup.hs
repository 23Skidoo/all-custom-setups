import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.UserHooks
import Distribution.Simple.Setup

import System.Directory
import System.Environment
import System.Process
import System.FilePath

main = defaultMainWithHooks simpleUserHooks { postConf = buildLibs }

buildLibs :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
buildLibs _ _ _ _ = do
  wd <- getCurrentDirectory
  let libOut = wd </> "build" </> "libs" </> "packages.a"
  let buildLib = shell ("go build -buildmode=c-archive -o " ++ libOut ++ " oden/packages/cmd")
  setEnv "GOPATH" (wd </> "go")
  putStrLn $ "Compiling Go library to " ++ libOut
  out <- readCreateProcess buildLib ""
  putStr out
  return ()
