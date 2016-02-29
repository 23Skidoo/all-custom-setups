import Distribution.Simple
import System(getArgs)
main = do (a:_) <- getArgs
          putStr $ if a == "build" then "\n ** Please see 000-readme in the distro tarball for building and usage. **\n\n" else ""
          defaultMain
