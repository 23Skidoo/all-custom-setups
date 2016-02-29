#!/usr/bin/env runhaskell
> import Distribution.PackageDescription
> import Distribution.Simple
> import Distribution.Simple.LocalBuildInfo
> import Distribution.Simple.Setup
> import System.FilePath
> import System.Directory
> import System.IO
> import Control.Monad
> import Text.Regex
>
> main = defaultMainWithHooks (simpleUserHooks { postCopy = myPostCopy, postInst = myPostInst } )
>
> myPostCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
> myPostCopy _ copyflags pkgdesc lbi = do
>   let dirs = absoluteInstallDirs pkgdesc lbi (fromFlag $ copyDest copyflags)
>   libExecHook dirs
>   insertPath dirs
>
> myPostInst :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
> myPostInst _ _ pkgdesc lbi = do
>   let dirs = absoluteInstallDirs pkgdesc lbi NoCopyDest
>   libExecHook dirs
>   insertPath dirs
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
> insertPath :: InstallDirs String -> IO ()
> insertPath dirs = do
>   let ddir = datadir dirs
>   let userConfigTemplate = ddir </> "etc" </> "bluetilerc_user_template"
>   let pathToSystemConfig = ddir </> "etc" </> "bluetilerc"
>   contents <- readFileStrict userConfigTemplate
>   let contentsPatched = subRegex (mkRegex "__PATH_TO_BLUETILERC__") contents pathToSystemConfig
>   writeFile userConfigTemplate contentsPatched
>
> -- taken from System.IO.Strict on Hackage
> readFileStrict :: FilePath -> IO String
> readFileStrict name = openFile name ReadMode >>= hGetContentsStrict
>
> hGetContentsStrict :: Handle -> IO String
> hGetContentsStrict h = hGetContents h >>= \s -> length s `seq` return s
