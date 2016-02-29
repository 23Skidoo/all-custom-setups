{- source partly taken from http://wewantarock.wordpress.com/tag/cabal/ -}
module Main (main) where

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemStdInOut, withTempFile)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Verbosity
import Distribution.PackageDescription
import Distribution.System
import System.FilePath
import System.Directory
import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import System.IO (hClose, hPutStr)
import System.Cmd (rawSystem)
import Data.Maybe (fromJust)
import Foreign (bitSize)
import System.Exit 

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks 
    { confHook = myConfHook 
    , buildHook = myBuildHook
    }
    
myConfHook pkg_descr flags = do
    let verbosity = fromFlag (configVerbosity flags)
    lbi <- confHook simpleUserHooks pkg_descr flags
    cfg <- (case buildOS of
                        Windows -> configureWindows 
                        OSX     -> configureOSX
                        _       -> configureUnix) verbosity
    let info  = emptyBuildInfo
              { ccOptions = confFlags cfg
              , extraLibs = confLibs cfg
              }
        descr = updatePackageDescription (Just info, []) (localPkgDescr lbi)
    return $ lbi { localPkgDescr = descr } 

myBuildHook pkg_descr local_bld_info user_hooks bld_flags =
    do
    let lib       = fromJust (library pkg_descr)
        lib_bi    = libBuildInfo lib
        custom_bi = customFieldsBI lib_bi
    case lookup "x-cc-name" custom_bi of
      Nothing -> buildHook simpleUserHooks pkg_descr local_bld_info user_hooks bld_flags
      Just cpp_name -> do
        let cpp_name  = fromJust (lookup "x-cc-name" custom_bi)
            c_srcs    = cSources lib_bi
            mbits     = bitSize (undefined :: Int)
            cc_opts   = ("-m" ++ show mbits) : "-S" : ccOptions lib_bi
            inc_dirs  = includeDirs lib_bi
            lib_dirs  = extraLibDirs lib_bi
            bld_dir   = buildDir local_bld_info
            prog      = ConfiguredProgram { programId = cpp_name, programVersion = Nothing,
                                            programDefaultArgs = [], programOverrideArgs = [],
                                            programLocation = FoundOnSystem { locationPath = cpp_name } }
        -- Compile C/C++ sources
        putStrLn $ "invoking my compile phase " ++ cpp_name
        objs <- mapM (compileCxx prog cc_opts inc_dirs bld_dir) c_srcs
        -- Remove C/C++ source code from the hooked build (don't change libs)
        let 
            lib_bi'    = lib_bi { cSources = map replaceWithAsm c_srcs }
            lib'       = lib    { libBuildInfo = lib_bi' }
            pkg_descr' = pkg_descr { library = Just lib' }
        -- The following line invokes the standard build behaviour
        putStrLn "Invoke default build hook"
        buildHook simpleUserHooks pkg_descr' local_bld_info user_hooks bld_flags

compileCxx :: ConfiguredProgram  -- ^ C/C++ compiler (gcc)
           -> [String]           -- ^ Compile options from Cabal and wxConfig
           -> [String]           -- ^ Include paths from Cabal and wxConfig
           -> FilePath           -- ^ Base output directory
           -> FilePath           -- ^ Path to source file
           -> IO FilePath        -- ^ Path to generated object code
compileCxx gcc opts incls out_path cxx_src = do
    let includes  = map ("-I" ++) incls
        -- out_path' = normalisePath out_path
        -- cxx_src'  = normalisePath cxx_src
        -- out_file  = out_path </> dropFileName cxx_src </>
        --                replaceExtension (takeFileName cxx_src) ".o"
        out_file = replaceWithAsm cxx_src
        out       = ["-c", cxx_src, "-o", out_file]
        -- opts     = opts ++ osCompileOpts
        do_it     = True -- needsCompiling cxx_src out_file
    when do_it $ createDirectoryIfMissing True (dropFileName out_file) >> 
                 runProgram verbose gcc (includes ++ opts ++ out)
    return out_file

replaceWithAsm p = replaceExtension p ".s"

type Flags = [String]
type Libs  = [String]

data ConfState = ConfState { confFlags :: Flags, confLibs :: Libs }

emptyCfg = ConfState [] []

addFlag :: String -> StateT ConfState IO ()
addFlag flag = modify $ \(ConfState fs ls) -> ConfState (flag:fs) ls

addLib :: String -> StateT ConfState IO ()
addLib lib = modify $ \(ConfState fs ls) -> ConfState fs (lib:ls)
   
configureWindows :: Verbosity -> IO ConfState
configureWindows verbosity = return emptyCfg

configureOSX :: Verbosity -> IO ConfState
configureOSX verbosity = return emptyCfg

configureUnix :: Verbosity -> IO ConfState
configureUnix verbosity = do
    cfg <- execStateT configureUnix' $ ConfState [] []
    when (verbosity > normal) $ putStrLn $ unlines $ 
        map unwords [ "Flags:" : confFlags cfg, "Libs:" : confLibs cfg ]
    return cfg
    where 
      check' test = check verbosity (performTest verbosity test)
      configureUnix' :: StateT ConfState IO ()
      configureUnix' = do
          checkLibDir "/usr/X11" `orelse`
            checkLibDir "/usr/X11R7" `orelse`
            checkLibDir "/usr/X11R6" `orelse`
            checkLibDir "/usr/X11R5" `orelse`
            checkLibDir "/opt/X11R6" `orelse`
            checkLibDir "/usr/X"
          check' progXrandr  "Xrandr"  ["-D_GLFW_HAS_XRANDR"]      ["Xrandr"] `orelse`
            check' progVidMode "VidMode" ["-D_GLFW_HAS_XF86VIDMODE"] ["Xxf86vm", "Xext"]
          check' progGlXGetProcAddress    "glXGetProcAddress"    ["-D_GLFW_HAS_GLXGETPROCADDRESS"]    [] `orelse`
            check' progGlXGetProcAddressARB "glXGetProcAddressARB" ["-D_GLFW_HAS_GLXGETPROCADDRESSARB"] [] `orelse`
            check' progGlXGetProcAddressEXT "glXGetProcAddressEXT" ["-D_GLFW_HAS_GLXGETPROCADDRESSEXT"] [] `orelse`
            check' progDlOpen               "dlopen"               ["-D_GLFW_HAS_DLOPEN"]               []
          check' progSysConf "sysconf" ["-D_GLFW_HAS_SYSCONF"] []
          check' progSysCtl  "sysctl"  ["-D_GLFW_HAS_SYSCTL"]  []
          return ()

f `orelse` g = f >>= \b -> if b then return b else g



checkLibDir :: String -> StateT ConfState IO Bool
checkLibDir dir = do
	result <- lift $ doesDirectoryExist (dir ++ "/lib")
	when (result) $ addFlag ("-I" ++ dir ++ "/include")
	return result

check :: Verbosity -> (Flags -> IO Bool) -> String -> Flags -> Libs -> StateT ConfState IO Bool
check verbosity performCheck name flags libs = do
    when (verbosity >= normal) $ lift . putStr $ "Checking for " ++ name ++ " support..."
    success <- get >>= lift . performCheck . confFlags
    when (verbosity >= normal) $ lift . putStrLn $ if success then "yes" else "no"
    when success $ mapM_ addFlag flags >> mapM_ addLib libs
    return success
    
performTest :: Verbosity -> String -> Flags -> IO Bool
performTest verbosity contents flags = do
    tmpDir  <- getTemporaryDirectory
    withTempFile tmpDir "glfw-test.c" $ \path inHandle -> 
      withTempFile tmpDir "glfw-test.o" $ \objPath outHandle -> do
        hClose outHandle
        hPutStr inHandle contents
        hClose inHandle
        (out, err, exitCode) <- rawSystemStdInOut verbosity "cc" (["-c", path, "-o", objPath] ++ flags) Nothing False
        return (exitCode == ExitSuccess)     

progXrandr = unlines
            ["#include <X11/Xlib.h>"
            ,"#include <X11/extensions/Xrandr.h>"
            ,"int main() {return 0;}"
            ]
    
progVidMode = unlines
            ["#include <X11/Xlib.h>"
            ,"#include <X11/extensions/xf86vmode.h>"
            ,"#if defined(__APPLE_CC__)"
            ,"#error Not supported under Mac OS X"
            ,"#endif"
            ,"int main() {return 0;}"
            ]
          
progGlXGetProcAddress = unlines
            ["#include <X11/Xlib.h>"
            ,"#include <GL/glx.h>"
            ,"#include <GL/gl.h>"
            ,"int main() {void *ptr=(void*)glXGetProcAddress(\"glFun\"); return 0;}"
            ]
          
progGlXGetProcAddressARB = unlines
            ["#include <X11/Xlib.h>"
            ,"#include <GL/glx.h>"
            ,"#include <GL/gl.h>"
            ,"int main() {void *ptr=(void*)glXGetProcAddressARB(\"glFun\"); return 0;}"
            ]
          
progGlXGetProcAddressEXT = unlines
            ["#include <X11/Xlib.h>"
            ,"#include <GL/glx.h>"
            ,"#include <GL/gl.h>"
            ,"int main() {void *ptr=(void*)glXGetProcAddressEXT(\"glFun\"); return 0;}"
            ]
    
progDlOpen = unlines
            ["#include <dlfcn.h>"
            ,"int main() {void *l=dlopen(\"libGL.so\",RTLD_LAZY|RTLD_GLOBAL); return 0;}"
            ]
    
progSysConf = unlines
            ["#include <unistd.h>"
            ,"#ifndef _SC_NPROCESSORS_ONLN"
            ,"#ifndef _SC_NPROC_ONLN"
            ,"#error Neither _SC_NPROCESSORS_ONLN nor _SC_NPROC_ONLN available"
            ,"#endif"
            ,"#endif"
            ,"int main() {long x=sysconf(_SC_ARG_MAX); return 0; }"
            ]
    
progSysCtl = unlines
            ["#include <sys/types.h>"
            ,"#include <sys/sysctl.h>"
            ,"#ifdef CTL_HW"
            ,"#ifndef HW_NCPU"
            ,"  error;"
            ,"#endif"
            ,"#endif"
            ,"int main() { return 0; }"
            ]

