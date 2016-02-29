import Distribution.Simple
         ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.LocalBuildInfo( LocalBuildInfo(..) )
import Distribution.PackageDescription
         ( PackageDescription(executables), Executable(buildInfo, exeName)
         , BuildInfo(customFieldsBI), emptyBuildInfo
         , updatePackageDescription, cppOptions, ccOptions )
import Distribution.Simple.Setup
    (buildVerbosity, copyDest, copyVerbosity, fromFlag,
     haddockVerbosity, installVerbosity, sDistVerbosity)
import Distribution.Verbosity
         ( Verbosity )
import System( system, exitWith )
import System.FilePath( (</>) )

-- for endianness check
import Foreign.Marshal.Utils ( with )
import Data.Word             ( Word8, Word32 )
import Foreign.Storable      ( peek )
import Foreign.Ptr           ( castPtr )

hst = "hashed-storage-test"

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
  runTests = \ _ _ _ lbi -> do
               exitWith =<< system (buildDir lbi </> hst </> hst),

  buildHook = \ pkg lbi hooks flags ->
              let verb = fromFlag $ buildVerbosity flags
               in commonBuildHook buildHook pkg lbi hooks verb >>= ($ flags),

  haddockHook = \ pkg lbi hooks flags ->
                let verb = fromFlag $ haddockVerbosity flags
                 in commonBuildHook haddockHook pkg lbi hooks verb >>= ($ flags)
}

commonBuildHook :: (UserHooks -> PackageDescription -> LocalBuildInfo -> t -> a)
                -> PackageDescription -> LocalBuildInfo -> t -> Verbosity -> IO a
commonBuildHook runHook pkg lbi hooks verbosity = do
  -- Add custom -DFOO[=BAR] flags to the cpp (for .hs) and cc (for .c)
  -- invocations, doing a dance to make the base hook aware of them.
  littleEndian <- testEndianness
  let args = if littleEndian then [ "-DLITTLEENDIAN" ] else [ "-DBIGENDIAN" ]
      bi = emptyBuildInfo { cppOptions = args, ccOptions = args }
      hbi = (Just bi, [(exeName exe, bi) | exe <- executables pkg])
      pkg' = updatePackageDescription hbi pkg
      lbi' = lbi { localPkgDescr = pkg' }
  return $ runHook simpleUserHooks pkg' lbi' hooks

  where
    testEndianness :: IO Bool
    testEndianness = with (1 :: Word32) $ \p -> do o <- peek $ castPtr p
                                                   return $ o == (1 :: Word8)
