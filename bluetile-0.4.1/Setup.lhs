#!/usr/bin/env runhaskell
> import Distribution.PackageDescription
> import Distribution.Simple
> import Distribution.Simple.LocalBuildInfo
> import Distribution.Simple.Setup
> import System.FilePath
> import System.Directory
> import Control.Monad
> main = defaultMainWithHooks (simpleUserHooks { postInst = myPostInst } )
>
> -- postInst hook to move helper binaries to the libexec directory
> myPostInst :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
> myPostInst _ _ pkgdesc lbi = do
>     let dirs = absoluteInstallDirs pkgdesc lbi NoCopyDest
>     let lexecdir = libexecdir dirs
>     createDirectoryIfMissing True lexecdir
>     forM_ ["bluetiledock", "bluetilemockwin", "bluetilegreet"] $ \binary ->
>       renameFile (bindir dirs </> binary) (lexecdir </> binary)
