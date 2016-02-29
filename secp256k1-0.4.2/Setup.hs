import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils

main :: IO ()
main = defaultMainWithHooks autoconfUserHooks
    { preConf = autogen
    , preBuild = make
    }

autogen :: Args -> ConfigFlags -> IO HookedBuildInfo
autogen _ cfg = do
    rawSystemExit v "./autogen.sh" []
    return emptyHookedBuildInfo
  where
    v = fromFlag $ configVerbosity cfg

make _ bi = do
    rawSystemExit v "make" ["src/ecmult_static_context.h"]
    return emptyHookedBuildInfo
  where
    v = fromFlag $ buildVerbosity bi
