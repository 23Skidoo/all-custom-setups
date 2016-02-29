import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.UserHooks
import Distribution.Simple.LocalBuildInfo
import System.FilePath
import System.Process

-- http://gbacon.blogspot.com/2009/06/setting-up-simple-test-with-cabal.html

main = defaultMainWithHooks hooks
  where hooks = simpleUserHooks { runTests = runTests' }

runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTests' _ _ _ lbi = system testprog >> return ()
  where testprog = buildDir lbi </> "heapsort-tests" </> "heapsort-tests"
