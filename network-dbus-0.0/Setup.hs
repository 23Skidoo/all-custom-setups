import Distribution.Simple
import Distribution.Simple.Utils (rawSystemExit)
import Distribution.Verbosity (normal)

main = defaultMainWithHooks $
    simpleUserHooks { runTests = runDBusTests }

runDBusTests _ _ _ _ = rawSystemExit normal "runhaskell" ["test.hs"]

