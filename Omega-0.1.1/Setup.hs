
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import qualified Distribution.Verbosity as Verbosity
import System.Cmd
import System.Directory
import System.Exit(ExitCode(..))
import System.IO
import System.FilePath((</>))
import System.Process

-- Mimic the && command of 'sh'
(>&&>) :: IO ExitCode -> IO ExitCode -> IO ExitCode
cmd1 >&&> cmd2 = do
  rc <- cmd1
  case rc of
    ExitSuccess   -> cmd2
    ExitFailure _ -> return rc

-- We will call 'autoconf' and 'make'
autoconfProgram = simpleProgram "autoconf"
makeProgram = simpleProgram "make"

-- Our single C++ source file is here
cppSourceName = "src" </> "C_omega.cc"

-- It becomes this object file
cppObjectName = "build" </> "C_omega.o"

-------------------------------------------------------------------------------
-- Configuration

configureOmega pkgDesc flags = do
  -- Run Cabal configure
  lbi <- confHook simpleUserHooks pkgDesc flags

  let verb = fromFlagOrDefault Verbosity.normal $ configVerbosity flags
      cfg = withPrograms lbi 

      runAutoconf = do rawSystemProgramConf verb autoconfProgram cfg []
                       return ExitSuccess
      
  -- Run autoconf configure
  runAutoconf >&&> runConfigure lbi

  return lbi

    where
      -- Run 'configure' with the extra arguments that were passed to
      -- Setup.hs
      runConfigure lbi = do
        currentDir <- getCurrentDirectory

        let opts = autoConfigureOptions lbi
            configProgramName = currentDir </> "configure"

        rawSystem configProgramName opts

-- Configuration: extract options to pass to 'configure'
autoConfigureOptions :: LocalBuildInfo -> [String]
autoConfigureOptions localBuildInfo = [libdirs, includedirs]
    where
      libraryDescr = case library $ localPkgDescr localBuildInfo
                     of Nothing -> error "Library description is missing"
                        Just l -> l

      buildinfo = libBuildInfo libraryDescr

      -- Create a string "-L/usr/foo -L/usr/bar"
      ldflagsString =
          intercalate " " ["-L" ++ dir | dir <- extraLibDirs buildinfo]

      libdirs = "LDFLAGS=" ++ ldflagsString

      -- Create a string "-I/usr/foo -I/usr/bar"
      cppflagsString =
          intercalate " " ["-I" ++ dir | dir <- includeDirs buildinfo]

      includedirs = "CPPFLAGS=" ++ cppflagsString

-------------------------------------------------------------------------------
-- Building

buildOmega pkgDesc lbi userhooks flags = do
  -- Do default build procedure for hs files
  buildHook simpleUserHooks pkgDesc lbi userhooks flags

  -- Get 'ar' program
  let verb = fromFlagOrDefault Verbosity.normal $ buildVerbosity flags
  (arPgm, _) <- requireProgram verb arProgram AnyVersion (withPrograms lbi)

  -- Build the C++ source file
  rawSystemProgramConf verb makeProgram (withPrograms lbi) ["all"]

  -- Add the object file to libraries
  let pkgId   = package $ localPkgDescr lbi

  let addStaticObjectFile objName libName =
          rawSystemProgram verb arPgm ["r", libName, objName]

  when (withVanillaLib lbi) $
       let libName = buildDir lbi </> mkLibName pkgId
       in addStaticObjectFile cppObjectName libName

  when (withProfLib lbi) $
       let libName = buildDir lbi </> mkProfLibName pkgId
       in addStaticObjectFile cppObjectName libName

  when (withSharedLib lbi) $
       die "Sorry, this package is not set up to build shared libraries"

  return ()

-------------------------------------------------------------------------------
-- Cleaning

cleanOmega pkgDesc mlbi userhooks flags = do
  let verb = fromFlagOrDefault Verbosity.normal $ cleanVerbosity flags

  -- Clean extra files if we don't need to save configuration
  -- (Other temp files are automatically cleaned)
  unless (fromFlag $ cleanSaveConf flags) $ do
    lenientRemoveFiles configFiles
    lenientRemoveDirectory "autom4te.cache"

  -- Do default clean procedure
  cleanHook simpleUserHooks pkgDesc mlbi userhooks flags

    where
      -- Attempt to remove a file, ignoring errors
      lenientRemoveFile f =
          removeFile f `catch` \_ -> return ()

      lenientRemoveFiles = mapM_ lenientRemoveFile

      -- Attempt to remove a directory and its contents
      -- (one level of recursion only), ignoring errors
      lenientRemoveDirectory f = do
        b <- doesDirectoryExist f
        when b $ do lenientRemoveFiles =<< getDirectoryContents f
                    removeDirectory f `catch` \_ -> return ()

      -- Extra files produced by configuration
      configFiles = ["configure", "config.log", "config.status", "Makefile"]

-------------------------------------------------------------------------------
-- Hooks

hooks =
    simpleUserHooks
    { hookedPrograms = [arProgram, autoconfProgram, makeProgram]
    , confHook = configureOmega
    , buildHook = buildOmega
    , cleanHook = cleanOmega
    }

main = defaultMainWithHooks hooks
