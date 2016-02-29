#!/usr/bin/env runhaskell
> import Distribution.Simple
> import Distribution.PackageDescription
> import Distribution.Simple.Setup ( defaultBuildFlags )
> import Distribution.Simple.LocalBuildInfo ( buildDir )
> import Distribution.Simple.Utils ( rawSystemExit )
> import Distribution.Verbosity ( normal )
> import System.FilePath ( (</>) )
> import Control.Monad ( forM_, when )
> import Data.Char ( toLower )

> main = defaultMainWithHooks $ handleTestExes simpleUserHooks

> handleTestExes hooks = hooks { instHook = dropTestExes instHook
>                              , copyHook = dropTestExes copyHook
>                              , runTests = runTestExes
>                              }
>     where
>       dropTestExes hook pkg =
>           let nonTest = filter (not . isTestExe) (executables pkg)
>           in hook hooks $ pkg { executables = nonTest }
>
>       runTestExes args _unknown pkg lbi = do
>         buildHook hooks pkg lbi hooks defaultBuildFlags
>         forM_ (executables pkg) $ \exe ->
>             let testPath = buildDir lbi </> exeName exe </> exeName exe
>             in when (isTestExe exe) $ rawSystemExit normal testPath args

> isTestExe exe =
>     let fields = customFieldsBI (buildInfo exe)
>     in ["true"] == [ map toLower val | ("x-test-executable", val) <- fields ]
