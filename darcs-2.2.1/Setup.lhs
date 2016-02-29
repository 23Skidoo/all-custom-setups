\begin{code}
{-# OPTIONS_GHC -cpp #-}
-- copyright (c) 2008 Duncan Coutts
-- portions copyright (c) 2008 David Roundy

import Distribution.Simple
         ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.PackageDescription
         ( PackageDescription(executables), Executable(buildInfo)
         , BuildInfo(customFieldsBI), emptyBuildInfo
         , updatePackageDescription )
import Distribution.Package
         ( packageVersion )
import Distribution.Simple.Program
         ( Program(..), simpleProgram, findProgramVersion
         , rawSystemProgramStdoutConf )
import Distribution.Simple.Configure
         ( ccLdOptionsBuildInfo )
import Distribution.Version
         ( Version(versionTags) )
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..) )

import Distribution.Simple.Setup
         ( configVerbosity, buildVerbosity, sDistVerbosity, fromFlag )
import Distribution.Simple.BuildPaths
         ( autogenModulesDir )
import Distribution.System
         ( OS(Windows), buildOS )
import Distribution.Simple.Utils
         ( rewriteFile, rawSystemStdout, createDirectoryIfMissingVerbose
         , withFileContents, notice )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text
         ( display )

import Control.Monad ( zipWithM_, when )
import Control.Exception ( bracket, bracket_ )
import System.Directory( doesDirectoryExist, doesFileExist,
                         getDirectoryContents, createDirectory,
                         copyFile, removeDirectoryRecursive,
                         getCurrentDirectory, setCurrentDirectory,
                         removeFile, createDirectoryIfMissing )
import System.IO.Error ( isDoesNotExistError )
import Data.List( isSuffixOf )
import System( system, ExitCode(..) )

import System.FilePath       ( (</>) )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable      ( peek )
import Foreign.Ptr           ( castPtr )
import Data.Word             ( Word8, Word32 )

import qualified Distribution.ShellHarness as Harness ( runTests )

#if __GLASGOW_HASKELL__ >= 610
import qualified Control.OldException as Exception
#else
import qualified Control.Exception as Exception
#endif

main = defaultMainWithHooks simpleUserHooks {

  hookedPrograms = [libwwwconfigProgram],
  
  confHook = \(pkg0, pbi) flags -> do
    let verbosity = fromFlag (configVerbosity flags)

    -- Call the ordinary build code
    lbi <- confHook simpleUserHooks (pkg0, pbi) flags

    -- Do some custom stuff:
    let pkg = localPkgDescr lbi
    libwwwBi <- getLibwwwBuildInfo verbosity pkg lbi
    let hbi  = (Nothing, [("darcs", libwwwBi)])
        lbi' = lbi { localPkgDescr = updatePackageDescription hbi pkg } 
    generateAutoconfModule verbosity pkg lbi
    return lbi',

  buildHook = \pkg lbi hooks flags -> do
    let verbosity = fromFlag (buildVerbosity flags)
    
    -- Do some custom stuff:
    writeGeneratedModules verbosity pkg lbi
    
    -- Call the ordinary build code
    buildHook simpleUserHooks pkg lbi hooks flags,

  runTests = \ args _ _ _ ->
             sequence_ [ case w of
                           x | x == "bugs" -> allTests Bug
                             | x == "network" -> execTests Network ""
                             | x == "tests" -> allTests Test
                             | otherwise -> fail $ "Unknown test: " ++ x
                         | w <- if null args then ["tests"] else args ],

  -- Remove the temporary directories created by "cabal test".
  postClean = \ _ _ _ _ -> mapM_ rmRf
              ["tests-darcs-2.dir",
               "tests-hashed.dir",
               "tests-old-fashioned-inventory.dir",
               "bugs-darcs-2.dir",
               "bugs-hashed.dir",
               "bugs-old-fashioned-inventory.dir",
               "tests_network-.dir"],

  sDistHook = \ pkg lbi hooks flags -> do
    let pkgVer = packageVersion pkg
        verb = fromFlag $ sDistVerbosity flags
    x <- versionPatches verb pkgVer
    y <- context verb pkgVer
    rewriteFile "release/distributed-version" $ show x
    rewriteFile "release/distributed-context" $ show y

    sDistHook simpleUserHooks pkg lbi hooks flags
}

libwwwconfigProgram :: Program
libwwwconfigProgram = (simpleProgram "libwww-config") {
    programFindVersion = findProgramVersion "--version" id
  }

getLibwwwBuildInfo verbosity pkg lbi
  | "x-have-libwww" `elem` customFields = do
    cflags <- libwwwconfig ["--cflags"]
    libs   <- libwwwconfig ["--libs"]
    return (ccLdOptionsBuildInfo (words cflags) (words libs))

  | otherwise = return emptyBuildInfo

  where
    libwwwconfig = rawSystemProgramStdoutConf verbosity
                     libwwwconfigProgram (withPrograms lbi)
    customFields = map fst . customFieldsBI . buildInfo $ darcsExe
    [darcsExe]   = executables pkg

generateAutoconfModule verbosity pkg lbi = do
  bigendian <- fmap not archIsLittleEndian

  let subst "configure_input" = targetFile ++ ". Generated from "
                                  ++ templateFile ++ " by Setup.hs."
      subst "HAVE_HTTP"     = show ("x-have-http" `elem` customFields)
      subst "USE_COLOR"     = show ("x-use-color" `elem` customFields)
      subst "USE_MMAP"     
                | isWindows = show False
                | otherwise = show True
      subst "HAVE_SENDMAIL" = show True
      subst "SENDMAIL"      = "/usr/sbin/sendmail"
      subst "HAVE_MAPI"
                | isWindows = show True
                | otherwise = show False
      subst "DIFF"          = "diff"
      subst "BIGENDIAN"     = show bigendian
      subst other           = unexpected other

  createDirectoryIfMissingVerbose verbosity True targetDir
  withFileContents templateFile
    (rewriteFile targetFile . templateSubstitute subst)

  where
    templateFile = "src/Autoconf.hs.in"
    targetDir    = autogenModulesDir lbi
    targetFile   = targetDir </> "Autoconf.hs"
    unexpected other = error $ "unexpected variable in template file "
                            ++ templateFile ++ ": " ++ show other
    isWindows    = case Distribution.System.buildOS of
                     Windows -> True
                     _       -> False
    customFields = map fst . customFieldsBI . buildInfo $ darcsExe
    [darcsExe]   = executables pkg

archIsLittleEndian :: IO Bool
archIsLittleEndian =
  with (1 :: Word32) $ \p -> do o <- peek $ castPtr p
                                return $ o == (1 :: Word8)

writeGeneratedModules :: Verbosity
                      -> PackageDescription -> LocalBuildInfo -> IO ()
writeGeneratedModules verbosity pkg lbi = do
  createDirectoryIfMissingVerbose verbosity True (autogenModulesDir lbi)

  let versionModulePath = autogenModulesDir lbi </> "ThisVersion.hs"
  generateVersionModule verbosity versionModulePath pkg

  let contextModulePath = autogenModulesDir lbi </> "Context.hs"
  generateContextModule verbosity contextModulePath pkg

generateVersionModule verbosity targetFile pkg = do
  let darcsVersion  =  packageVersion pkg
  numPatches <- versionPatches verbosity darcsVersion
  let darcsVersionState = versionStateString numPatches darcsVersion
      subst "DARCS_VERSION"       = display darcsVersion
      subst "DARCS_VERSION_STATE" = darcsVersionState
      subst other                 = unexpected other
  
  withFileContents templateFile
    (rewriteFile targetFile . templateSubstitute subst)
      
  where
    versionStateString :: Maybe Int -> Version -> String
    versionStateString Nothing  _ = "unknown" 
    versionStateString (Just 0) v = case versionTags v of
                         ["pre"] -> "prerelease"
                         ["rc"]  -> "release candidate"
                         []      -> "release"
                         _       -> "tag"
    versionStateString (Just 1) _ = "+ 1 patch"
    versionStateString (Just n) _ = "+ " ++ show n ++ " patches"
    templateFile = "src/ThisVersion.hs.in"
    unexpected other = error $ "unexpected variable in template file "
                            ++ templateFile ++ ": " ++ show other

versionPatches :: Verbosity -> Version -> IO (Maybe Int)
versionPatches verbosity darcsVersion = do
  numPatchesDarcs <- do
      out <- rawSystemStdout verbosity "darcs"
               ["changes", "--from-tag", display darcsVersion, "--count"]
      case reads (out) of
        ((n,_):_) -> return $ Just ((n :: Int) - 1)
        _         -> return Nothing
    `Exception.catch` \_ -> return Nothing

  numPatchesDist <- parseFile versionFile
  return $ case (numPatchesDarcs, numPatchesDist) of
             (Just x, _) -> Just x
             (Nothing, Just x) -> Just x
             (Nothing, Nothing) -> Nothing

 where
  versionFile = "release/distributed-version"

generateContextModule verbosity targetFile pkg = do
  ctx <- context verbosity (packageVersion pkg)
  rewriteFile targetFile $ unlines
    ["module Context where"
    ,"context :: String"
    ,"context = " ++ case ctx of
                       Just x -> show x
                       Nothing -> show "context not available"
    ]

context :: Verbosity -> Version -> IO (Maybe String)
context verbosity version = do
  contextDarcs <- do
      -- FIXME currently we run changes --from-tag to at least assert that the
      -- requested version is tagged in this repository... it is a weak check,
      -- but otherwise, my ~/_darcs context tends to gets used when running
      -- from an unpacked distribution
      rawSystemStdout verbosity "darcs"
                          ["changes", "--from-tag", display version ]
      out <- rawSystemStdout verbosity "darcs" ["changes", "--context"]
      return $ Just out
   `Exception.catch` \_ -> return Nothing

  contextDist <- parseFile contextFile
  return $ case (contextDarcs, contextDist) of
             (Just x, _) -> Just x
             (Nothing, Just x) -> Just x
             (Nothing, Nothing) -> Nothing
 where contextFile = "release/distributed-context"

parseFile :: (Read a) => String -> IO (Maybe a)
parseFile f = do
  exist <- doesFileExist f
  if exist then do
             content <- readFile f -- ^ ratify readFile: we don't care here.
             case reads content of
               ((s,_):_) -> return s
               _         -> return Nothing
             else return Nothing

-------------------------------------
-- Running the testsuite
--

data TestKind = Bug | Test | Network deriving Eq

instance Show TestKind where
    show Bug = "bugs"
    show Test = "tests"
    show Network = "tests/network"

flat a = [ if x == '/' then '_' else x | x <- show a ]

harness :: String
harness = "perl ../tests/shell_harness"

isTest :: FilePath -> Bool
isTest = (".sh" `isSuffixOf`)

execTests' :: TestKind -> IO ()
execTests' k =
    do fs <- getDirectoryContents "."
       cwd <- getCurrentDirectory
       let run = filter isTest fs
       res <- Harness.runTests cwd run
       when ((not res) && (k /= Bug)) $ fail "Tests failed"
       return ()

execTests :: TestKind -> String -> IO ()
execTests k fmt = do
  copyFile "dist/build/darcs/darcs" "darcs"
  let dir = (flat k) ++ "-" ++ fmt ++ ".dir"
  rmRf dir
  cloneTree (show k) dir
  withCurrentDirectory dir $ do
    createDirectory ".darcs"
    when (not $ null fmt) $ appendFile ".darcs/defaults" $ "ALL " ++ fmt ++ "\n"
    execTests' k

allTests :: TestKind -> IO ()
allTests k =
    do test `mapM` repotypes
       return ()
    where repotypes = ["darcs-2", "hashed", "old-fashioned-inventory"]
          test = execTests k

----------------------
-- Utility functions
--

templateSubstitute :: (String -> String) -> String -> String
templateSubstitute varSubst = subst
  where
    subst text = case span (/= '@') text of
      (chunk, []) -> chunk
      (chunk, '@':rest) -> case span (/= '@') rest of
        (var, '@':rest) -> chunk ++ varSubst var ++ subst rest

-------------------------------------------------------
-- More utility functions (FIXME)
-- copy & paste & edit: darcs wants to share these
--

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory name m =
    bracket
        (do cwd <- getCurrentDirectory
            when (name /= "") (setCurrentDirectory name)
            return cwd)
        (\oldwd -> setCurrentDirectory oldwd `catch` (\_ -> return ()))
        (const m)

cloneTree :: FilePath -> FilePath -> IO ()
cloneTree = cloneTreeExcept []

cloneTreeExcept :: [FilePath] -> FilePath -> FilePath -> IO ()
cloneTreeExcept except source dest =
 do isdir <- doesDirectoryExist source
    if isdir then do
        createDirectoryIfMissing True dest
        fps <- getDirectoryContents source
        let fps' = filter (`notElem` (".":"..":except)) fps
            mk_source fp = source ++ "/" ++ fp
            mk_dest   fp = dest   ++ "/" ++ fp
        zipWithM_ cloneSubTree (map mk_source fps') (map mk_dest fps')
     else fail ("cloneTreeExcept: Bad source " ++ source)
   `catch` fail ("cloneTreeExcept: Bad source " ++ source)

cloneSubTree :: FilePath -> FilePath -> IO ()
cloneSubTree source dest =
 do isdir <- doesDirectoryExist source
    isfile <- doesFileExist source
    if isdir then do
        createDirectory dest
        fps <- getDirectoryContents source
        let fps' = filter (`notElem` [".", ".."]) fps
            mk_source fp = source ++ "/" ++ fp
            mk_dest   fp = dest   ++ "/" ++ fp
        zipWithM_ cloneSubTree (map mk_source fps') (map mk_dest fps')
     else if isfile then do
        cloneFile source dest
     else fail ("cloneSubTree: Bad source "++ source)
    `catch` (\e -> if isDoesNotExistError e
                   then return ()
                   else ioError e)

cloneFile :: FilePath -> FilePath -> IO ()
cloneFile = copyFile

rmRf path = do
  isdir <- doesDirectoryExist path
  isf <- doesFileExist path
  when isdir $ removeDirectoryRecursive path
  when isf $ removeFile path
  return ()

-- (END FIXME)

\end{code}
