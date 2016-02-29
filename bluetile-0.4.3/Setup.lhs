#!/usr/bin/env runhaskell
> import Distribution.PackageDescription
> import Distribution.Simple
> import Distribution.Simple.LocalBuildInfo
> import Distribution.Simple.Setup
> import System.FilePath
> import System.Directory
> import Control.Monad
> main = defaultMainWithHooks (simpleUserHooks { postCopy = myPostCopy, postInst = myPostInst } )
>
> myPostCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
> myPostCopy _ copyflags pkgdesc lbi = do
>   let dirs = absoluteInstallDirs pkgdesc lbi (fromFlag $ copyDest copyflags)
>   libExecHook dirs
>
> myPostInst :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
> myPostInst _ _ pkgdesc lbi = do
>   let dirs = absoluteInstallDirs pkgdesc lbi NoCopyDest
>   libExecHook dirs
>
> -- hook to move helper binaries to the libexec directory
> libExecHook :: InstallDirs String -> IO ()
> libExecHook dirs = do
>   let bdir = bindir dirs
>   let lexecdir = libexecdir dirs
>   createDirectoryIfMissing True lexecdir
>   forM_ ["bluetiledock", "bluetilemockwin", "bluetilegreet"] $ \binary ->
>     renameFile (bdir </> binary) (lexecdir </> binary)
