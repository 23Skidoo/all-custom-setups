#!/usr/bin/env runhaskell
> import Distribution.PackageDescription
> import Distribution.Simple
> import Distribution.Simple.LocalBuildInfo
> import Distribution.Simple.Setup
> import System.FilePath
> import System.Directory
> import System.IO
> import Control.Monad
> import Data.List
>
> main = defaultMainWithHooks (simpleUserHooks { postCopy = myPostCopy, postInst = myPostInst } )
>
> myPostCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
> myPostCopy _ copyflags pkgdesc lbi = do
>   let copyDirs = absoluteInstallDirs pkgdesc lbi (fromFlag $ copyDest copyflags)
>   libExecHook copyDirs
>   let installDirs = absoluteInstallDirs pkgdesc lbi NoCopyDest
>   insertPath copyDirs installDirs
>
> myPostInst :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
> myPostInst _ _ pkgdesc lbi = do
>   let dirs = absoluteInstallDirs pkgdesc lbi NoCopyDest
>   libExecHook dirs
>   insertPath dirs dirs
>
> -- hook to move helper binaries to the libexec directory
> libExecHook :: InstallDirs String -> IO ()
> libExecHook dirs = do
>   let bdir = bindir dirs
>   let lexecdir = libexecdir dirs
>   createDirectoryIfMissing True lexecdir
>   forM_ ["bluetiledock", "bluetilemockwin", "bluetilegreet"] $ \binary ->
>     renameFile (bdir </> binary) (lexecdir </> binary)
>
> -- hook to insert path to system wide configuration
> insertPath :: InstallDirs String -> InstallDirs String -> IO ()
> insertPath copyDirs installDirs = do
>   let userConfigTemplate = datadir copyDirs </> "etc" </> "bluetilerc_user_template"
>   let pathToSystemConfig = datadir installDirs </> "etc" </> "bluetilerc"
>   contents <- readFileStrict userConfigTemplate
>   let contentsPatched = replaceStr "__PATH_TO_BLUETILERC__" pathToSystemConfig contents
>   writeFile userConfigTemplate contentsPatched
>
> replaceStr :: (Eq a) => [a] -> [a] -> [a] -> [a]
> replaceStr _ _ [] = []
> replaceStr old new xs@(y:ys) =
>   case stripPrefix old xs of
>       Nothing -> y : replaceStr old new ys
>       Just ys' -> new ++ replaceStr old new ys'
>
> -- taken from System.IO.Strict on Hackage
> readFileStrict :: FilePath -> IO String
> readFileStrict name = openFile name ReadMode >>= hGetContentsStrict
>
> hGetContentsStrict :: Handle -> IO String
> hGetContentsStrict h = hGetContents h >>= \s -> length s `seq` return s
