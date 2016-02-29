#!/usr/bin/env runhaskell
> import Distribution.Simple
> import System.Process (rawSystem)
> import System.Exit (ExitCode(..))
> main = defaultMainWithHooks $ simpleUserHooks { runTests = \args _ _ _ -> do
>     ExitSuccess <- rawSystem "runhaskell" ("Test.hs" : args)
>     return ()
> }
