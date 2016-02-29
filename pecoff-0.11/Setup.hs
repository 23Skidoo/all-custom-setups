import Distribution.Simple
import System.Cmd(system)

main = defaultMainWithHooks $ simpleUserHooks { runTests = runPecoffTests }

runPecoffTests a b pd lb = system "runhaskell -i./src ./tests/Test.hs" >> return ()
