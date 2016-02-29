import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.Utils
import Distribution.Simple.Program
import Distribution.Verbosity
import System.Process
import System.Directory
import System.Exit
import Text.Groom

main = defaultMainWithHooks hk
 where
 hk = simpleUserHooks { buildHook = \pd lbi uh bf -> do
                                        print "yo yo yo"
                                        let ccProg = Program "gcc" undefined undefined undefined
                                            mConf = lookupProgram ccProg (withPrograms lbi)
                                            err = error "Could not determine C compiler"
                                            cc = locationPath . programLocation  . maybe err id $ mConf
                                        b <- canUseAesIntrinsicsFlag cc
                                        print (withPrograms lbi)
                                        let newWithPrograms1 = userSpecifyArgs "gcc" aesArgs (withPrograms lbi)
                                            newWithPrograms  = userSpecifyArgs "ghc" aesArgsHC newWithPrograms1
                                        buildHook simpleUserHooks pd (lbi {withPrograms = newWithPrograms }) uh bf
                      }

aesArgs :: [String]
aesArgs = ["-maes", "-mssse3", "-DHAVE_AES_INTRINSICS"]

aesArgsHC :: [String]
aesArgsHC = map ("-optc" ++) aesArgs

canUseAesIntrinsicsFlag :: FilePath -> IO Bool
canUseAesIntrinsicsFlag cc = do
        withTempDirectory normal "" "testIntrinsic" $ \tmpDir -> do
        writeFile (tmpDir ++ "/testIntrinsic.c")
                (unlines        [ "#include <wmmintrin.h>"
                                , "int main() {"
                                , "return 0; }"
                                ])
        ec <- rawSystemExitCode normal cc ["-maes",tmpDir ++ "/testIntrinsic.c"]
        notice normal $ "Result of NI Intrinsics Test: " ++ show (ec == ExitSuccess)
        return (ec == ExitSuccess)