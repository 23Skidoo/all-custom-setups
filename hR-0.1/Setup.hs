import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.Setup
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.ModuleName (components)
import Control.Monad
import Data.Maybe
import System.Directory
import System.FilePath
defhooks = autoconfUserHooks
programs = 
  [ (simpleProgram "R") {
      programFindVersion = findProgramVersion "--version" $ \str ->
	case words str of
	    ("R":"version":ver:_) -> ver
	    _ -> ""
    }
  ]
postconf args flags desc build = do
  confExists <- doesFileExist "configure"
  unless confExists $ rawSystemExit verb "autoconf" []
  postConf defhooks args flags{ configConfigureArgs = configConfigureArgs flags ++ confargs } desc build
  where 
    verb = fromFlag $ configVerbosity flags
    confargs = map pconfarg pconf
    pconfarg p = "--with-" ++ programId p ++ "=" ++ programPath p ++ " " ++ unwords (programDefaultArgs p ++ programOverrideArgs p)
    pconf = mapMaybe (\p -> lookupProgram p (withPrograms build)) programs
hooks = defhooks 
  { hookedPrograms = programs
  , postConf = postconf
  }
main = defaultMainWithHooks hooks
