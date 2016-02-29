#!/usr/bin/env runhaskell
> import Distribution.Simple
> import Distribution.PackageDescription
> import Distribution.Simple.Setup ( defaultBuildFlags )
> import Distribution.Simple.LocalBuildInfo ( buildDir )
> import Distribution.Simple.Utils ( rawSystemExit )
> import Distribution.Verbosity ( normal )
> import System.FilePath ( (</>) )

> main = defaultMainWithHooks hooks

> hooks = simpleUserHooks { instHook = inst
>                         , runTests = test
>                         }

> inst pkg = instHook simpleUserHooks $ pkg { executables = [] }

> test args _unknown pkg lbi = do
>   buildHook hooks pkg lbi hooks defaultBuildFlags
>   let testPath = buildDir lbi </> "test" </> "test"
>   rawSystemExit normal testPath args