import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import System.Cmd
import System.Exit

main = defaultMainWithHooks hooks
  where hooks = simpleUserHooks { runTests = runTests' }

runTests' args _ _ lbi = do
  exitCode <- rawSystem "runhaskell" $
              [ "-odir=" ++ testobj
              , "-hidir=" ++ testobj
              , "-fobject-code"
              , "Test.hs"
              ] ++ args
  case exitCode of
    ExitFailure _ -> exitWith exitCode
    _ -> return ()
  where testobj = buildDir lbi ++ "/testobj"
