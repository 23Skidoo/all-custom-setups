#!/usr/bin/env runhaskell
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo

-- Import the local test suite
import Tests.PL (test)

runTest :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTest _ _ _descr _buildInfo = test

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
    runTests = runTest
  }
