import Distribution.Simple
         ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.LocalBuildInfo( LocalBuildInfo(..) )
import System( system )
import System.FilePath( (</>) )

main = defaultMainWithHooks simpleUserHooks {
  runTests = \ _ _ pkg lbi -> do
               system $ buildDir lbi </> hst </> hst
               return ()
}
    where hst = "hashed-storage-test"
