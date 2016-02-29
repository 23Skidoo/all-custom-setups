import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo, buildDir)
import System.Cmd(system)
import System.FilePath

main = defaultMainWithHooks hooks
  where hooks = simpleUserHooks { runTests = runTests' }

runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTests' _ _ _ lbi = system testprog >> return ()
  where testprog = (buildDir lbi) </> "test" </> "test"