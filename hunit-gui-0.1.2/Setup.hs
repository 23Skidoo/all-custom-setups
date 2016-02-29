import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import System.Cmd
import System.Exit


main :: IO ()
main = defaultMainWithHooks (simpleUserHooks { runTests = runAllTests })

runAllTests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runAllTests _ _ _ _ = do
  code <- system "./dist/build/tests/tests"
  exitWith code
