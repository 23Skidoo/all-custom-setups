-- |
-- There is a bug in 'text' before 0.11.2.3 with Int8 'decimal' function,
-- minor bug, actually, but it broke tests. So we can't use cabal's MIN_VERSION
-- define because of it doesn't account for four number version.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.PackageIndex
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.BuildPaths
import Distribution.Simple.Setup

import System.FilePath

buildUserHook :: PackageDescription -> LocalBuildInfo
              -> UserHooks -> BuildFlags -> IO ()
buildUserHook pkgDescr lbi hooks flags = do
    (buildHook simpleUserHooks) newPkgDescr lbi hooks flags
  where
    -- Bug havd been fixed in 0.11.2.3 version
    lastBadVersion = Version [0, 11, 2, 2] []
    package = PackageName "text"

    -- Suppose that least installed version used
    packageIndex = installedPkgs lbi
    minVersion = minimum $ map fst $ lookupPackageName packageIndex package

    -- Add CPP BADTEXT define, so we can check it in tests
    newOptions options
        | minVersion <= lastBadVersion = "-DBADTEXT" : options
        | otherwise = options

    -- Update options only in test suites
    newPkgDescr = pkgDescr { testSuites = newTestSuites }
    -- Create new test suites with updated options
    newTestSuites = map updateTest $ testSuites pkgDescr
    -- Change options in every single test suite
    updateTest t@TestSuite { testBuildInfo = b@BuildInfo { .. } } =
        t { testBuildInfo = b { cppOptions = newOptions cppOptions } }

hooks :: UserHooks
hooks = simpleUserHooks { buildHook = buildUserHook }

main :: IO ()
main = defaultMainWithHooks hooks
