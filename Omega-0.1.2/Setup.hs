
import Control.Applicative
import Control.Exception(bracket)
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
import System.FilePath((</>), (<.>), takeExtension)
import System.Process

-- Mimic the && command of 'sh'
(>&&>) :: IO ExitCode -> IO ExitCode -> IO ExitCode
cmd1 >&&> cmd2 = cmd1 >>= continue
    where 
      continue ExitSuccess = cmd2
      continue returnCode  = return returnCode

-- Record whether we're building the Omega library here
useInstalledOmegaFlagPath = "build" </> "UseInstalledOmega"

writeUseInstalledOmegaFlag :: Bool -> IO ()
writeUseInstalledOmegaFlag b = do
  createDirectoryIfMissing False "build"
  writeFile useInstalledOmegaFlagPath (show b)

readUseInstalledOmegaFlag :: IO Bool
readUseInstalledOmegaFlag = do
  text <- readFile useInstalledOmegaFlagPath `catch`
          \_ -> die "Configuration file missing; try reconfiguring"
  return $! read text

-- We will call 'autoconf' and 'make'
autoconfProgram = simpleProgram "autoconf"
makeProgram = simpleProgram "make"

-- Our single C++ source file and corresponding object file are here
cppSourceName = "src" </> "C_omega.cc"
cppObjectName = "build" </> "C_omega.o"

-- If we're building the Omega library, it's here
omegaLibPath = "src" </> "the-omega-project" </> "omega_lib" </> "obj" </> "libomega.a"

-- Unpack the Omega library into this directory
omegaUnpackPath = "build" </> "unpack_omega"

noGHCiLib =
    die "Sorry, this package does not support GHCi.\n\
        \Please configure with --disable-library-for-ghci to disable."

noSharedLib =
    die "Sorry, this package does not support shared library output.\n\
        \Please configure with --disable-shared to disable."

-------------------------------------------------------------------------------
-- Configuration

configureOmega pkgDesc flags = do
  -- Run Cabal configure
  lbi <- confHook simpleUserHooks pkgDesc flags

  -- Detect and report error on unsupported configurations
  when (withGHCiLib lbi) noGHCiLib

  when (withSharedLib lbi) noSharedLib

  let verb = fromFlagOrDefault Verbosity.normal $ configVerbosity flags
      cfg = withPrograms lbi
      runAutoconf = do rawSystemProgramConf verb autoconfProgram cfg []
                       return ExitSuccess
      
  -- Run autoconf configure
  runAutoconf >&&> runConfigure lbi

  -- Save this flag for later use
  writeUseInstalledOmegaFlag useInstalledOmega

  return lbi

    where
      -- Will build the Omega library?
      useInstalledOmega = fromMaybe False $
                          lookup (FlagName "useinstalledomega") $
                          configConfigurationsFlags flags

      -- Run 'configure' with the extra arguments that were passed to
      -- Setup.hs
      runConfigure lbi = do
        currentDir <- getCurrentDirectory

        let opts = autoConfigureOptions lbi useInstalledOmega
            configProgramName = currentDir </> "configure"

        rawSystem configProgramName opts

-- Configuration: extract options to pass to 'configure'
autoConfigureOptions :: LocalBuildInfo -> Bool -> [String]
autoConfigureOptions localBuildInfo useInstalledOmega =
    withOmega ++ [libdirs, includedirs]
    where
      withOmega = if useInstalledOmega
                  then ["--with-omega"]
                  else []

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

  useInstalledOmega <- readUseInstalledOmegaFlag

  -- Do default build procedure for hs files
  buildHook simpleUserHooks pkgDesc lbi userhooks flags

  -- Get 'ar' and 'ld' programs
  let verb = fromFlagOrDefault Verbosity.normal $ buildVerbosity flags
  (arPgm, _) <- requireProgram verb arProgram AnyVersion (withPrograms lbi)
  let runAr = rawSystemProgram verb arPgm

  -- Build the C++ source file (and Omega library, if configured)
  -- Makefile's behavior is controlled by output of 'configure'
  rawSystemProgramConf verb makeProgram (withPrograms lbi) ["all"]

  -- Add other object files to libraries
  let pkgId   = package $ localPkgDescr lbi

  let -- Add extra files into an archive file
      addStaticObjectFiles libName = do
          -- Add the C++ interface file
          addStaticObjectFile cppObjectName libName

          -- Add contents of libomega.a
          unless useInstalledOmega $
              transferArFiles verb runAr omegaLibPath libName

          where
            addStaticObjectFile objName libName =
                runAr ["r", libName, objName]

  when (withVanillaLib lbi) $
       let libName = buildDir lbi </> mkLibName pkgId
       in addStaticObjectFiles libName

  when (withProfLib lbi) $
       let libName = buildDir lbi </> mkProfLibName pkgId
       in addStaticObjectFiles libName

  when (withGHCiLib lbi) noGHCiLib
  when (withSharedLib lbi) noSharedLib

  return ()

-- Transfer the contents of one archive to another
transferArFiles verb runAr src dst = do
  srcCan <- canonicalizePath src
  dstCan <- canonicalizePath dst
  withTempDirectory verb omegaUnpackPath $

    -- Save/restore the current working directory
    bracket getCurrentDirectory setCurrentDirectory $ \_ -> do

      -- Go to temporary directory
      setCurrentDirectory omegaUnpackPath

      -- Unpack source archive
      runAr ["x", srcCan]

      -- Find object files
      objs <- liftM (filter isObjectFile) $ getDirectoryContents "."
      when (null objs) $ warn verb "No object files found in Omega library; build may be incomplete"

      -- Insert into destination archive
      runAr (["r", dstCan] ++ objs)
    where
      isObjectFile f = takeExtension f == ".o"

-------------------------------------------------------------------------------
-- Cleaning

cleanOmega pkgDesc mlbi userhooks flags = do
  let verb = fromFlagOrDefault Verbosity.normal $ cleanVerbosity flags

  -- run 'make clean', which will clean the Omega library if appropriate
  case mlbi of
    Just lbi -> rawSystemProgramConf verb makeProgram (withPrograms lbi) ["clean"]
    Nothing -> return ()

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
        when b $ do lenientRemoveFiles . map (f </>) =<< getDirectoryContents f
                    removeDirectory f `catch` \_ -> return ()

      -- Extra files produced by configuration
      configFiles = ["configure", "config.log", "config.status", "Makefile",
                     useInstalledOmegaFlagPath]

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
