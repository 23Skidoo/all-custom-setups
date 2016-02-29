{-# LANGUAGE RecordWildCards #-}
import Distribution.Verbosity 
import Distribution.Simple.Setup
import Distribution.Simple.BuildPaths
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.Find
import Distribution.Simple.Program.Db

import System.FilePath ((</>), (<.>))
import System.Directory

import Control.Exception
import Control.Monad

confProgram, makeProgram :: Program
confProgram = simpleProgram "configure"
makeProgram = simpleProgram "make"

main :: IO ()
main = do 
    curDir <- getCurrentDirectory
    let confProgram' = confProgram
            { programFindLocation = \_ _ -> return . Just $  curDir </> "libxls-0.3.0_pre107" </> "configure"
            }

    defaultMainWithHooks simpleUserHooks 
        { confHook = xlsConfHook
        , buildHook = myBuildHook
        , copyHook = xlsCopyHook
        , hookedPrograms = hookedPrograms simpleUserHooks ++ [confProgram', makeProgram]
        }

myBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
myBuildHook pkg lbi usrHooks flags = do
    curDir <- getCurrentDirectory


    let verbosity = fromFlag $ buildVerbosity flags
        library' = case library pkg of
            Just l -> Just $ l { libBuildInfo = (libBuildInfo l) { extraLibs = "xlsreader" : []
                                                                 , extraLibDirs = [curDir </> "lib"]
                                                                 }
                                } 
            Nothing -> library pkg
        pkg' = pkg { library = library' }
    
        lbi' = case lookupProgram ghcProgram (withPrograms lbi) of
                Just ghc -> 
                    let pdb = updateProgram (ghc { programOverrideArgs = ["-optl-Wl,-rpath," ++ libdir (absoluteInstallDirs pkg lbi NoCopyDest)] }) (withPrograms lbi)
                    in
                        lbi { withPrograms = pdb }
                Nothing -> lbi

    inDir (curDir </> "build") $
        runDbProgram verbosity makeProgram (withPrograms lbi) ["install"]
    
    let xlslib = "lib" ++ "xlsreader" <.> dllExtension
    isLibExist <- doesFileExist $ curDir </> "lib" </> xlslib
    unless isLibExist $ die "Error Compiling libxls !!!"

    buildHook simpleUserHooks pkg' lbi' usrHooks flags

    notice verbosity "Making The STATIC Library"
    let mylib = curDir </> "lib" </> mkStaticLib "xlsreader"
    forM_ (libName lbi') $ \lib -> do
        inDir (buildDir lbi) $ do
            runAr verbosity lbi ["-M"] $ arScript ([mylib, lib]) lib

xlsConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
xlsConfHook (pkg, hbi) flags = do
    let verbosity = fromFlag (configVerbosity flags)
    curDir <- getCurrentDirectory
    lbi <- confHook simpleUserHooks (pkg, hbi) flags

    let xlsRepoDir = curDir </> "libxls-0.3.0_pre107"
        xlsBuildDir = curDir </> "build"
        xlsPfxDir = xlsBuildDir </> "dist"
        xlsLibDir = curDir </> "lib"

    createDirectoryIfMissingVerbose verbosity True xlsBuildDir
    createDirectoryIfMissingVerbose verbosity True xlsPfxDir
    createDirectoryIfMissingVerbose verbosity True xlsLibDir
    notice verbosity "Configuring libxls ..."

    xlsExist <- doesFileExist "libxls-0.3.0_pre107.tar.bz2"
    unless xlsExist $ die "No libxls source tar package found"
    runTar verbosity lbi ["axvf", "libxls-0.3.0_pre107.tar.bz2"]

    inDir xlsBuildDir $ 
        runDbProgram verbosity confProgram (withPrograms lbi)
            [ "--libdir=" ++ xlsLibDir
            , "--prefix=" ++ xlsPfxDir 
            ]

    return lbi

xlsCopyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
xlsCopyHook pkg lbi usrHooks flags = do
  copyHook simpleUserHooks pkg lbi usrHooks flags

  curDir <- getCurrentDirectory
  let xlsLibDir = curDir </> "lib"
      verbosity = fromFlag (copyVerbosity flags)
      destdir = fromFlagOrDefault NoCopyDest $ copyDest flags
      libCopyDir = libdir $ absoluteInstallDirs pkg lbi destdir

  notice verbosity $ "Installing libxls shared libraries (" ++ show libCopyDir ++ ") ..."
  copyFiles verbosity libCopyDir [(xlsLibDir, "lib" ++ "xlsreader" <.> dllExtension ++ ".0")]

mkStaticLib :: String -> String
mkStaticLib name = mkLibName (LibraryName name)

runTar :: Verbosity -> LocalBuildInfo -> [String] -> IO ()
runTar v lbi args = do
    case lookupProgram tarProgram (withPrograms lbi) of
        Nothing -> die "No <tar command> found!"
        Just ar -> runProgramInvocation v $ (programInvocation ar args)

runAr :: Verbosity -> LocalBuildInfo -> [String] -> String -> IO ()
runAr v lbi args script = do
    case lookupProgram arProgram (withPrograms lbi) of
        Nothing -> die "No <ar> found!"
        Just ar -> runProgramInvocation v $ (programInvocation ar args) { progInvokeInput = Just script }

arScript :: [String] -> String -> String
arScript libs name = unlines $ ["CREATE " ++ name]
                            ++ map ("ADDLIB " ++) libs
                            ++ ["SAVE", "END"]

inDir :: FilePath -> IO () -> IO ()
inDir dir act = do
    curDir <- getCurrentDirectory
    bracket_ (setCurrentDirectory dir)
             (setCurrentDirectory curDir)
             act

libName :: LocalBuildInfo -> [String]
libName lbi = 
    concatMap pick (componentsConfigs lbi)
  where
    pick :: (ComponentName, ComponentLocalBuildInfo, [ComponentName]) -> [String]
    pick (_, LibComponentLocalBuildInfo {..}, _) = map mkLibName $ componentLibraries   
    pick _ = []
