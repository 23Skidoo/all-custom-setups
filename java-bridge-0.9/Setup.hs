{-# LANGUAGE Haskell2010
    , ScopedTypeVariables
    , CPP
 #-}
{-# OPTIONS
    -Wall
    -fno-warn-missing-signatures
    -fno-warn-name-shadowing
 #-}

import Prelude
import qualified Prelude as P

import qualified Control.Exception as Exc
import Control.Monad (when)

import Data.List

import Distribution.PackageDescription
import Distribution.System
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import Distribution.Verbosity

import System.Environment
import System.Exit (ExitCode (..))
import System.Directory
import System.FilePath

import Text.Printf

import Language.Preprocessor.Cpphs

#if linux_HOST_OS == 1 || darwin_HOST_OS == 1
import System.Posix.Files
#endif

main = do
    let verb = verbose

    args <- getArgs

    args' <- case args of
        "configure" : _ -> do
            notice verb ("OS=" ++ show buildOS)
            notice verb ("ARCH=" ++ show buildArch)
            args_ <- Exc.catch (configure verb args buildOS) $ \exc -> do
                warn verb $
                    "Could not find a JDK. Continuing anyway.\n"
                 ++ "The error is: "
                 ++ ((show :: Exc.SomeException -> String) exc)
                return $
                    args ++ ["--extra-include-dirs=./include",
                             "--ghc-option=-optc-DFFIJNI_LIBJVM=!404!"]
            return args_
        _ -> return args

    let hooks = simpleUserHooks {
            hookedPreProcessors = [ ("tpl", transformTpl)
                                  , ("hss", transformHss)
                                  , ("cpphs", transformCpphs) ]
          }

    defaultMainWithHooksArgs hooks args'


transformCpphs :: BuildInfo -> LocalBuildInfo -> PreProcessor
transformCpphs _ _ = PreProcessor True $ \(inDir, inFile) (outDir, outFile) _ -> do
    source <- readFile (inDir </> inFile)

    _ <- printf "Preprocessing (CPPHS) %s -> %s\n"
        (inDir </> inFile) (outDir </> outFile)

    let options = defaultCpphsOptions {
            boolopts = (defaultBoolOptions { ansi = True })
          }

    runCpphs options (inDir </> inFile) source
        >>= writeFile (outDir </> outFile)


transformTpl :: BuildInfo -> LocalBuildInfo -> PreProcessor
transformTpl _ _ = PreProcessor True $ \(inDir, inFile) (outDir, outFile) _ -> do
    let (baseDir, _) = splitFileName (inDir </> inFile)
    template <- readFile (inDir </> inFile)

    let (templateFile, params) = read (filter (/= '\n') template)
            :: (String, [(String, String)])

    _ <- printf "Preprocessing (TPL) %s -> %s (using %s)\n"
            (inDir </> inFile) (outDir </> outFile) templateFile

    template <- readFile (baseDir ++ templateFile)

    writeFile (outDir </> outFile) (processTpl params template)

processTpl params xss = case xss of
    ('%':xs) -> let (var, (_:rest)) = break (== '%') xs
                in  maybe "" id (lookup var params) ++ processTpl params rest
    (x:xs) -> x : processTpl params xs
    "" -> ""


transformHss :: BuildInfo -> LocalBuildInfo -> PreProcessor
-- ^ Transforms a *.hss file containing TriString-literals ("""triString""")
-- into an ordinary *.hs file.
transformHss _ _ = PreProcessor True $ \(inDir, inFile) (outDir, outFile) _ -> do
    source <- readFile (inDir </> inFile)

    _ <- printf "Preprocessing (HSS) %s -> %s\n"
            (inDir </> inFile) (outDir </> outFile)

    writeFile (outDir </> outFile) (doProcessHss inFile source)

-- | The actual processing of 'transformHss'. No advanced method of parsing
-- have been used in order to reduce the dependencies of the Setup script
-- and to now blow it up by a whole combinator library or auto generated code.
--
-- Every triString is replaced by the concatenation of ordinary haskell
-- strings. Code embedded in curly braces (introduced by a hash sign) is
-- embedded literally. LINE pragmas keep the line numbers consistent.
doProcessHss file = processHss (1 :: Int)
  where
    processHss n xs = case xs of
        ('"':'"':'"':xs) -> triString n xs
        ('{':'-':xs) -> '{' : '-' : comment n (1 :: Int) xs
        ('"':xs) -> '"' : string n xs
        ('\n':xs) -> '\n' : processHss (succ n) xs
        (x:xs) -> x : processHss n xs
        "" -> ""
      where
        triString n xs = case breakIt xs of
            (str, xs) -> concat [ "((\\_ -> let { __ = "
                , "\n{-# LINE ", show n, " ", show file, " #-}\n"
                , sanitizeIt (mkTriString str)
                , "\n{-# LINE ", show n', " ", show file, " #-}\n"
                , "} in __) undefined)"
                , processHss n' xs ]
              where n' = n + length (filter (== '\n') str)

        breakIt xs = case xs of
            ('"':'"':'"':xs) -> ("", xs)
            (x:xs) -> case breakIt xs of (y, ys) -> (x : y, ys)
            "" -> ("", "")

        sanitizeIt xs = case xs of
            ('\\':'\\':'n':xs) -> sanitizeIt xs
            (x:xs) -> x : sanitizeIt xs
            "" -> ""

        mkTriString xs = "concat [\"" ++ process string ++ "\"]"
          where
            string = concat $ intersperse "\\n" lines
            process xs = case xs of
                ('#':'{':xs) -> case break (== '}') xs of
                    (ls, (_:rs)) -> "\", (" ++ ls ++ "), \"" ++ process rs
                    _ -> error "could not match closing curly brace"
                ('"':xs) -> '\\' : '"' : process xs
                (x:xs) -> x : process xs
                [] -> []
            lines = case P.lines xs of
                ("":lines) -> map (dedent (indent lines)) lines
                (line:[]) -> [line]
                lines@(_:lines') -> map (dedent (indent lines')) lines
                [] -> []
            indent xs = case xs of
                [] -> 0
                _ -> minimum $ map (length . takeWhile (== ' ')) xs
            dedent n line = case splitAt n line of
                (ls, rs) -> dropWhile (== ' ') ls ++ rs

        comment l 1 ('-':'}':xs) = '-' : '}' : processHss l xs
        comment l n xs = case xs of
            ('{':'-':xs) -> '{' : '-' : comment l (succ n) xs
            ('-':'}':xs) -> '-' : '}' : comment l (pred n) xs
            ('\n':xs) -> '\n' : comment (succ l) n xs
            (x:xs) -> x : comment l n xs
            "" -> ""

        string l xs = case xs of
            ('\\':'"':xs) -> '\\' : '"' : string l xs
            ('"':xs) -> '"' : processHss l xs
            (x:xs) -> x : string l xs
            "" -> ""

    
configure verb args OSX = do
    -- In Mac OS X there is the tool /usr/libexec/java_home which tells
    -- us where JRE and JDK are installed.

    javaHome <- findFirstFile id ["/usr/libexec/java_home"]
        >>= maybe (fail "Could not determine JAVA_HOME using /usr/libexec/java_home - is there a JDK installed?")
                  return
        >>= (\cmd -> rawSystemStdout silent cmd [])
        >>= return . head . lines

    libjvmPath <- getEnvironment >>= return . lookup "FFIJNI_LIBJVM"
        >>= maybe (return $ javaHome ++ "/jre/lib/server/libjvm.dylib") return

    notice verb $ "JAVA_HOME=" ++ javaHome
    notice verb $ "FFIJNI_LIBJVM=" ++ libjvmPath

    return $ args ++ ["--extra-include-dirs=" ++ javaHome ++ "/include",
                      "--extra-include-dirs=" ++ javaHome ++ "/include/darwin",
                      "--ghc-option=-optc-DFFIJNI_LIBJVM=" ++ libjvmPath]


configure verb args Linux = do
    -- In Linux we find out where javac lives and resolve all
    -- symlinks until we find the JDK home.

    let lookupJavac = findExecutable "javac"
            >>= maybe (fail "Could not determine path to javac. Try setting JAVA_HOME.") return
            >>= resolve >>= return . takeDirectory . takeDirectory

    javaHome <- getEnvironment >>= return . lookup "JAVA_HOME"
        >>= maybe lookupJavac return

    when (verb >= deafening) $ do
        getDirectoryContentsRecursive javaHome >>= mapM_ (debug verb)

    rawSystemExitCode normal (javaHome ++ "/bin/javac") ["GetProperty.java"]
        >>= (\exit -> case exit of
                ExitSuccess -> return ()
                ExitFailure code -> fail $ "javac exited with ExitCode " ++ show code)

    javaArch <- rawSystemStdout normal (javaHome ++ "/bin/java") ["GetProperty", "os.arch"]
	    >>= return . head . lines

    notice verb $ "os.arch=" ++ javaArch

    libjvmPath <- getEnvironment >>= return . lookup "FFIJNI_LIBJVM"
        >>= maybe (return $ javaHome ++ "/jre/lib/" ++ javaArch ++ "/server/libjvm.so") return

    notice verb $ "JAVA_HOME=" ++ javaHome
    notice verb $ "FFIJNI_LIBJVM=" ++ libjvmPath

    findFirstFile id [javaHome ++ "/include/jni.h"]
        >>= maybe (fail $ "jni.h was not found in " ++ javaHome ++ "/include")
                  (return . const ())

    findFirstFile id [javaHome ++ "/include/linux/jni_md.h"]
        >>= maybe (fail $ "jni_md.h was not found in " ++ javaHome ++ "/include/linux")
                  (return . const ())

    return $ args ++ ["--extra-include-dirs=" ++ javaHome ++ "/include",
                      "--extra-include-dirs=" ++ javaHome ++ "/include/linux",
                      "--ghc-option=-optc-DFFIJNI_LIBJVM=" ++ libjvmPath]


configure verb args Windows = do
    -- The strategy for finding a JDK in Windows is rather simplistic.
    -- Typically the JDK is installed in %ProgramFiles%/Java, so we look
    -- into that directory for directories starting with @jdk@. We choose
    -- the one with the highest version number (that's what sort is for).

    programFiles <- getEnv "ProgramFiles"

    javaHome <- getDirectoryContents (programFiles ++ "\\Java")
        >>= return . (programFiles ++) . ("\\Java\\" ++) . last . sort . filter ("jdk" `isPrefixOf`)

    libjvmPath <- getEnvironment >>= return . lookup "FFIJNI_LIBJVM"
        >>= maybe (return $ javaHome ++ "\\jre\\bin\\server\\jvm.dll") return
        >>= return . replace '\\' '/'

    notice verb $ "JAVA_HOME=" ++ javaHome
    notice verb $ "FFIJNI_LIBJVM=" ++ libjvmPath

    findFirstFile id [javaHome ++ "/include/jni.h"]
        >>= maybe (fail $ "jni.h was not found in " ++ javaHome ++ "/include")
                  (return . const ())

    return $ args ++ ["--extra-include-dirs=" ++ javaHome ++ "/include",
                      "--extra-include-dirs=" ++ javaHome ++ "/include/win32",
                      "--ghc-option=-optc-DFFIJNI_LIBJVM=" ++ libjvmPath]

configure verb args os = do
    warn verb $ "Unknown platform: " ++ show os
    -- default, nothing
    return args

replace :: Char -> Char -> String -> String
replace needle replacement haystack = case haystack of
    (x:xs) -> (if x == needle then replacement else x) : replace needle replacement xs
    [] -> []

resolve :: FilePath -> IO FilePath
#if linux_HOST_OS == 1 || darwin_HOST_OS == 1
resolve filePath = do
    stat <- getSymbolicLinkStatus filePath
    if isSymbolicLink stat
        then readSymbolicLink filePath >>= resolve
        else return filePath
#else
-- in Windows there are no Symlinks, thus resolve does nothing.
resolve = return
#endif

