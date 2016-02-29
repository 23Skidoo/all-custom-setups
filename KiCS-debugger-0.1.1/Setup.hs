module Main where

import System.FilePath
import System.Directory
import Monad
import Data.List

import Distribution.Simple
import Distribution.Simple.Setup 
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.PackageDescription 
import Distribution.Verbosity
import Distribution.ModuleName hiding (main)

import Curry.Files.CymakePath
import Curry.Files.KiCSPath
import Curry.Files.ProphecyPath

withLibs :: Bool
withLibs = False

kics, cymake, prophecy :: IO Program
kics     = mkProg getKiCS
cymake   = mkProg getCymake
prophecy = mkProg getProphecy

mkProg getProg = do
  call <- getProg
  return (simpleProgram (takeBaseName call))
            {programFindLocation = \_-> return (Just call)}

mkstrict :: Program
mkstrict = simpleProgram "mkstrict"

main :: IO ()
main = do
  cymakeProg   <- cymake
  kicsProg     <- kics
  prophecyProg <- prophecy

  defaultMainWithHooks simpleUserHooks
    {hookedPrograms=mkstrict:kicsProg:cymakeProg:prophecyProg:
                    hookedPrograms simpleUserHooks
    ,confHook=myConfHook
    ,postConf=myPostConf
    ,hookedPreProcessors=[("curry",mkModule),
                          ("lcurry",mkModule),
                          ("fcy",mkOracleModule)]
    }

-- what a hack! something was forgotten in Distribution.ModuleName
modName :: [String] -> ModuleName
modName xs = read $ "ModuleName " ++ show xs

myConfHook :: 
  (Either GenericPackageDescription PackageDescription, HookedBuildInfo) 
    -> ConfigFlags -> IO LocalBuildInfo
myConfHook info flags = do
  lbi <- confHook simpleUserHooks info flags
  if not withLibs 
   then return lbi 
   else do
    print 13
    libPath <- getKiCSLibDir 
    allFiles <- getDirectoryContents libPath

    let stdfiles  = filter (isSuffixOf ".fcy") allFiles
        goodfiles = filter (not . (`elem` badlibs) . takeBaseName) stdfiles
        stdlibs   = map takeBaseName stdfiles
        goodlibs  = map takeBaseName goodfiles
        libs = map (\ l -> modName ("Curry":"Module":["Oracle" ++ l])) stdlibs
             ++map (\ l -> modName ("Curry":"DebugModule":[l])) goodlibs 

    let dbgPath = "Curry" </> "DebugModule"

    mapM_ (putStrLn . ("    "++) . foldr (<.>) "" . components) libs

    mapM_ (\ lib -> mayCopyFile (libPath </> lib) (dbgPath </> lib)) goodfiles
    
    let require = requireProg flags lbi 
    prop <- require prophecy
    mapM_ (mkOracleLib (unflag $ configVerbosity flags) prop) stdlibs
    
    dataPath <- getKiCSDataDir

    let package  = localPkgDescr lbi
        Just lib = library package
        ex       = exposedModules lib
        llbi     = libBuildInfo lib
        hsdirs   = hsSourceDirs llbi
        llbi'    = llbi{hsSourceDirs=dataPath:hsdirs}
        lib'     = Just lib{exposedModules=ex ++ libs,
                            libBuildInfo=llbi'}

    return (lbi{localPkgDescr=package{library=lib'}})


mayCopyFile :: FilePath -> FilePath -> IO ()
mayCopyFile src dest = do
  ex <- doesFileExist dest
  if ex then return () else copyFile src dest

unflag = fromFlagOrDefault silent

requireProg :: ConfigFlags -> LocalBuildInfo -> IO Program 
            -> IO ConfiguredProgram
requireProg verb lbi prog = do
  p <- prog
  (cp,_) <- requireProgram (unflag $ configVerbosity verb) p AnyVersion 
                           (withPrograms lbi)
  return cp
 
callProg :: Verbosity -> LocalBuildInfo -> IO Program -> Args -> IO ()
callProg verb lbi prog args = do
  p <- prog
  rawSystemProgramConf verb p (withPrograms lbi) args

mkOracleLib :: Verbosity -> ConfiguredProgram -> FilePath -> IO ()
mkOracleLib verb prop stdlib = do
  rawSystemProgram verb prop ["-o","Curry/Module/",stdlib]
  return ()

myPostConf :: 
  Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostConf args confFlags descrs lbi = do
  let require = requireProg confFlags lbi
  require cymake
  require kics
  require prophecy
  postConf simpleUserHooks args confFlags descrs lbi

badlibs =
  [
   -- mkstrict fails
   "Traversal"
  ,"FlatCurry"
  ,"AbstractCurry"
  ,"SetRBT"
  ,"TableRBT"
  ,"Pretty"
  ,"CurryStringClassifier"
  ,"FiniteMap"
  ,"Parser"
  ,"EventOracle"
  ,"CurrySyntax"
  ,"EasyCheck"

  -- consequence of FlatCurry failing
  ,"FlexRigid","FlatCurryTools","FlatCurryShow","FlatCurryRead"
  ,"FlatCurryXML","PrettyFlat","CompactFlatCurry","FlatCurryGoodies"

  -- consequence of FiniteMap failing
  ,"GraphInductive"

  -- consequence of AbstractCurry failing
  ,"AbstractCurryPrinter"

  -- consequence of Pretty failing
  ,"StyledText"

  -- resulting haskell incorrect
  ,"RedBlackTree"
  ,"IO"
  ,"Global"
  ,"Array"
  ,"Meta"

  -- consequence of incorrect IO
  , "Assertion"
  , "Oracle"
  , "Distribution"
  , "CEventOracle"
  , "IOExts"
  , "PropertyFile"
  , "Socket"

  -- consequence of incorrect Meta
  , "Unsafe"  

  -- consequence of incorrect Global
  , "System" 
  , "Random" 
  ]

mkModule :: BuildInfo -> LocalBuildInfo -> PreProcessor
mkModule _ lbi = PreProcessor
  {platformIndependent = True
  ,runPreProcessor     = mkSimplePreProcessor $ \ inf outf verb -> do
                          datadir <- getKiCSLibDir 
                          runKics datadir lbi inf outf verb}

runKics :: FilePath -> LocalBuildInfo 
        -> FilePath -> FilePath -> Verbosity -> IO ()
runKics datadir lbi infile outfile verb 
  | isPrefixOf "Curry/Module" infile
  = do callCymake ["-iCurry/Module","--no-hidden-subdir"]
       callKics   ["-userlibpath","Curry/Module"]
  | isPrefixOf "prophecy" infile
  = do callCymake ["-iprophecy/Curry/Module"]
       callKics   $ ["-userlibpath","prophecy/Curry/Module"] ++
         if isSuffixOf "Transform.curry" infile 
         then ["-executable","prophecy"]
         else []
  | isPrefixOf "biosphere" infile
  = do callCymake ["-iprophecy/Curry/Module","-ibiosphere/src/Curry/Module"]
       callKics $ ["-userlibpath",
                   "prophecy/Curry/Module:biosphere/src/Curry/Module"] ++
         if isSuffixOf "TransformationDependencies.lcurry" infile 
         then ["-executable","mkstrict"]
         else []
  | otherwise = error $ "runKics, unexpected infile: " ++ infile
 where
    call = callProg verb lbi
    callCymake args = 
      call cymake $ args ++ ["-i"++datadir,"-e","--flat",infile]
    callKics args = 
      call kics $ args ++ ["-nomake","-o",outfile,infile]

mkOracleModule :: BuildInfo -> LocalBuildInfo -> PreProcessor
mkOracleModule buildInfo lbi = PreProcessor
  {platformIndependent = True
  ,runPreProcessor     = mkSimplePreProcessor (runKicsFcy lbi)}

runKicsFcy :: LocalBuildInfo -> FilePath -> FilePath -> Verbosity -> IO ()
runKicsFcy lbi infile outfile verb 
  | isPrefixOf "Curry/Module" infile
  = do 
    libPath <- getOracleLibBaseDir
    callKics ["-userlibpath",libPath ++ pathSeparator:"Curry/Module"]
  | isPrefixOf "Curry/DebugModule" infile
  = do
    let dir = addTrailingPathSeparator $ 
              joinPath $
              reverse $ drop 2 $ reverse $
              splitDirectories (takeDirectory outfile)
    call (return mkstrict) ["-o",dir,dropExtension infile]
  
  where
    call = callProg verb lbi
    callKics args = 
      call kics $ args ++ ["-nomake","--no-interfaces","-o",outfile,infile]
  


