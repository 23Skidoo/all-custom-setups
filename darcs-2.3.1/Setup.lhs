\begin{code}
{-# OPTIONS_GHC -cpp #-}
-- copyright (c) 2008 Duncan Coutts
-- portions copyright (c) 2008 David Roundy

import Distribution.Simple
         ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.PackageDescription
         ( PackageDescription(executables), Executable(buildInfo, exeName)
         , BuildInfo(customFieldsBI), emptyBuildInfo
         , updatePackageDescription, cppOptions, ccOptions )
import Distribution.Package
         ( packageVersion )
import Distribution.Version
         ( Version(versionBranch) )
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), absoluteInstallDirs )
import Distribution.Simple.InstallDirs (mandir, CopyDest (NoCopyDest))
import Distribution.Simple.Setup
    (buildVerbosity, copyDest, copyVerbosity, fromFlag,
     haddockVerbosity, installVerbosity, sDistVerbosity)
import Distribution.Simple.BuildPaths
         ( autogenModulesDir )
import Distribution.System
         ( OS(Windows), buildOS )
import Distribution.Simple.Utils
    (copyFiles, createDirectoryIfMissingVerbose, rawSystemStdout,
     rewriteFile)
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text
         ( display )
import Distribution.Package (Package)

import Control.Monad ( zipWithM_, when, unless, filterM )
import Control.Exception ( bracket )
import System.Directory
    (copyFile, createDirectory, createDirectoryIfMissing,
     doesDirectoryExist, doesFileExist,
     getCurrentDirectory, getDirectoryContents,
     removeDirectoryRecursive, removeFile, setCurrentDirectory)
import System.IO (openFile, IOMode (..))
import System.Process (runProcess)
import System.IO.Error ( isDoesNotExistError )
import Data.List( isSuffixOf, sort, partition )

import System.FilePath       ( (</>), splitDirectories, isAbsolute )
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

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {

  buildHook = \ pkg lbi hooks flags ->
              let verb = fromFlag $ buildVerbosity flags
               in commonBuildHook buildHook pkg lbi hooks verb >>= ($ flags),

  haddockHook = \ pkg lbi hooks flags ->
                let verb = fromFlag $ haddockVerbosity flags
                 in commonBuildHook haddockHook pkg lbi hooks verb >>= ($ flags) ,

  postBuild = \ _ _ _ lbi -> buildManpage lbi,
  postCopy = \ _ flags pkg lbi ->
             installManpage pkg lbi (fromFlag $ copyVerbosity flags) (fromFlag $ copyDest flags),
  postInst = \ _ flags pkg lbi ->
             installManpage pkg lbi (fromFlag $ installVerbosity flags) NoCopyDest,

  runTests = \ args _ _ lbi -> do
             cwd <- getCurrentDirectory
             let isabs = isAbsolute $ buildDir lbi
                 path = (if isabs then id else (cwd </>))
                        (buildDir lbi </> "darcs")
                 what = if null args then ["tests"] else args
                 (series, tests) = partition
                                     (`elem` ["bugs", "network", "tests"]) what
             sequence_ [ case w of
                           "bugs" -> allTests path Bug []
                           "network" -> execTests path Network "" []
                           "tests" -> allTests path Test []
                           _ -> return () {- impossible, silence -Wall -}
                         | w <- series ]
             when (not $ null tests) $ individualTests path tests,

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

-- | For @./Setup build@ and @./Setup haddock@, do some unusual
-- things, then invoke the base behaviour ("simple hook").
commonBuildHook :: (UserHooks -> PackageDescription -> LocalBuildInfo -> t -> a)
                -> PackageDescription -> LocalBuildInfo -> t -> Verbosity -> IO a
commonBuildHook runHook pkg lbi hooks verbosity = do
  -- Autoconf may have generated a context file.  Remove it before
  -- building, as its existence inexplicably breaks Cabal.
  removeFile "src/Context.hs"
    `catch` (\e -> unless (isDoesNotExistError e) (ioError e))

  -- Create our own context file.
  writeGeneratedModules verbosity pkg lbi

  -- Add custom -DFOO[=BAR] flags to the cpp (for .hs) and cc (for .c)
  -- invocations, doing a dance to make the base hook aware of them.
  (version, state) <- determineVersion verbosity pkg
  littleEndian <- testEndianness
  let args = ("-DPACKAGE_VERSION=" ++ show' version) :
             ("-DPACKAGE_VERSION_STATE=" ++ show' state) :
             [arg | (arg, True) <-         -- include fst iff snd.
              [("-DHAVE_HTTP", "x-have-http" `elem` customFields),
               ("-DUSE_COLOR", "x-use-color" `elem` customFields),
               -- We have MAPI iff building on/for Windows.
               ("-DHAVE_MAPI", buildOS == Windows),
               ("-DBIGENDIAN", not littleEndian)]]
      bi = emptyBuildInfo { cppOptions = args, ccOptions = args }
      hbi = (Just bi, [(exeName exe, bi) | exe <- executables pkg])
      pkg' = updatePackageDescription hbi pkg
      lbi' = lbi { localPkgDescr = pkg' }
  return $ runHook simpleUserHooks pkg' lbi' hooks

  where
    customFields = map fst . customFieldsBI . buildInfo $ darcsExe
    darcsExe = head [e | e <- executables pkg, exeName e == "darcs"]
    show' :: String -> String   -- Petr was worried that we might
    show' = show                -- allow non-String arguments.
    testEndianness :: IO Bool
    testEndianness = with (1 :: Word32) $ \p -> do o <- peek $ castPtr p
                                                   return $ o == (1 :: Word8)

buildManpage :: LocalBuildInfo -> IO ()
buildManpage lbi = do
  let darcs = buildDir lbi </> "darcs/darcs"
      manpage = buildDir lbi </> "darcs/darcs.1"
  manpageHandle <- openFile manpage WriteMode
  runProcess darcs ["help","manpage"]
             Nothing Nothing Nothing (Just manpageHandle) Nothing
  return ()

installManpage :: PackageDescription -> LocalBuildInfo
                  -> Verbosity -> CopyDest -> IO ()
installManpage pkg lbi verbosity copy =
    copyFiles verbosity
              (mandir (absoluteInstallDirs pkg lbi copy) </> "man1")
              [(buildDir lbi </> "darcs", "darcs.1")]

writeGeneratedModules :: Verbosity
                      -> PackageDescription -> LocalBuildInfo -> IO ()
writeGeneratedModules verbosity pkg lbi = do
  createDirectoryIfMissingVerbose verbosity True (autogenModulesDir lbi)

  let contextModulePath = autogenModulesDir lbi </> "Context.hs"
  generateContextModule verbosity contextModulePath pkg

determineVersion :: Verbosity -> PackageDescription -> IO (String, String)
determineVersion verbosity pkg = do
  let darcsVersion  =  packageVersion pkg
  numPatches <- versionPatches verbosity darcsVersion
  return (display darcsVersion, versionStateString numPatches darcsVersion)

  where
    versionStateString :: Maybe Int -> Version -> String
    versionStateString Nothing  _ = "unknown"
    versionStateString (Just 0) v = case versionBranch v of
                         x | 97 `elem` x -> "alpha " ++ show (after 97 x)
                           | 98 `elem` x -> "beta " ++ show (after 98 x)
                           | 99 `elem` x  ->
                               "release candidate " ++ show (after 99 x)
                         _ -> "release"
    versionStateString (Just 1) _ = "+ 1 patch"
    versionStateString (Just n) _ = "+ " ++ show n ++ " patches"
    after w (x:r) | w == x = head r
                  | otherwise = after w r
    after _ [] = undefined

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

generateContextModule :: (Package pkg) => Verbosity -> FilePath -> pkg -> IO ()
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

flat :: (Show a) => a -> String
flat a = [ if x == '/' then '_' else x | x <- show a ]

isTest :: FilePath -> Bool
isTest = (".sh" `isSuffixOf`)

execTests :: FilePath -> TestKind -> String -> [String] -> IO ()
execTests darcs_path k fmt tests = do
  let dir = (flat k) ++ "-" ++ fmt ++ ".dir"
  rmRf dir
  cloneTree (show k) dir
  withCurrentDirectory dir $ do
    createDirectory ".darcs"
    when (not $ null fmt) $ appendFile ".darcs/defaults" $ "ALL " ++ fmt ++ "\n"
    putStrLn $ "Running tests for format: " ++ fmt
    exec
 where exec = do
         fs <- case tests of
                  [] -> sort `fmap` getDirectoryContents "."
                  x -> return x
         cwd <- getCurrentDirectory
         let run = filter isTest fs
         res <- Harness.runTests (Just darcs_path) cwd run
         when ((not res) && (k /= Bug)) $ fail "Tests failed"
         return ()

individualTests :: FilePath -> [String] -> IO ()
individualTests darcs_path tests = do
  run <- concat `fmap` mapM find tests
  sequence_ [ do exec kind [test | (kind', test) <- run, kind' == kind]
                     | kind <- [Test, Bug, Network] ]
      where tryin w t' = [w </> t', w </> (t' ++ ".sh")]
            exec _ [] = return ()
            exec kind to_run = allTests darcs_path kind to_run
            find t = do
              let c = [t, t ++ ".sh"] ++ tryin "tests" t ++ tryin "bugs" t
                        ++ tryin "network" t
              run <- map kindify `fmap` filterM doesFileExist c
              return $ take 1 run
            kindify test = case splitDirectories test of
                             [p, y] -> (parse_kind p, y)
                             _ -> error $ "Bad format in " ++ test ++
                                          ": expected type/test"
            parse_kind "tests" = Test
            parse_kind "bugs" = Bug
            parse_kind "network" = Network
            parse_kind x = error $ "Test prefix must be one of " ++
                           "[tests, bugs, network] in " ++ x


allTests :: FilePath -> TestKind -> [String] -> IO ()
allTests darcs_path k s =
    do test `mapM` repotypes
       return ()
    where repotypes = ["darcs-2", "hashed", "old-fashioned-inventory"]
          test x = execTests darcs_path k x s

-------------------------------------------------------
-- Utility functions (FIXME)
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

rmRf :: FilePath -> IO ()
rmRf path = do
  isdir <- doesDirectoryExist path
  isf <- doesFileExist path
  when isdir $ removeDirectoryRecursive path
  when isf $ removeFile path
  return ()

-- (END FIXME)

\end{code}
