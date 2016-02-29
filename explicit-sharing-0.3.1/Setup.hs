import System.Process
import System.Exit
import Distribution.Simple

main = defaultMainWithHooks $ simpleUserHooks { runTests = runTestSuite }

runTestSuite _ _ _ _ =
 do pid <- runCommand "ghc -hide-package monads-fd -e main Test.hs"
    waitForProcess pid >>= exitWith
