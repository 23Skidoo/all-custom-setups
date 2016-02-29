import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(withPrograms), buildDir)
import Distribution.Simple.Program (userSpecifyArgs)

import System.Directory
import System.FilePath

-- Define __HADDOCK__ when building documentation.
main = defaultMainWithHooks simpleUserHooks {
  haddockHook = \pkg lbi h f -> do
    let progs = userSpecifyArgs "hsc2hs" ["-D__HADDOCK__"] (withPrograms lbi)
    removePreProcessedFiles (buildDir lbi)
    haddockHook simpleUserHooks pkg lbi { withPrograms = progs } h f
}

-- Horrible hack to force re-processing of the .hsc file.  Otherwise
-- the __HADDOCK__ macro doesn't end up being defined.
removePreProcessedFiles :: FilePath -> IO ()
removePreProcessedFiles dir = do
  putStrLn $ "Trying to remove source in: " ++ dir
  removeFile (dir </> "System/BSD/Sysctl.hs")
    `catch` \_ -> putStrLn "Could not find source file!" >> return ()
