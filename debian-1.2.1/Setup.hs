#!/usr/bin/runhaskell

import Distribution.Simple (defaultMainWithHooks, defaultUserHooks, runTests, Args())
import Distribution.PackageDescription (PackageDescription())
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo())
import System.Exit (ExitCode())
import System.Cmd (system)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Control.Exception (finally)

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path f = do
        cur <- getCurrentDirectory
        setCurrentDirectory path
        finally f (setCurrentDirectory cur)

runTestScript :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTestScript args flag pd lbi = withCurrentDirectory "tests" (system "runhaskell Main.hs") >> return ()

main :: IO ()
main = defaultMainWithHooks defaultUserHooks{runTests = runTestScript}
