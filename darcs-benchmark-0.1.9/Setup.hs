import Distribution.Simple
         ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.LocalBuildInfo( LocalBuildInfo(..) )
import System( system, exitWith )
import System.FilePath( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
  runTests = \ _ _ _ lbi -> do
               exitWith =<< system (buildDir lbi </> "darcs-benchmark" </> "darcs-benchmark")
}
