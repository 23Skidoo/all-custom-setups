#! /usr/bin/env runhaskell

 
> import System.Cmd
> import System.Directory
> import System.FilePath

> import Distribution.Simple.UserHooks
> import Distribution.Simple.Setup
> import Distribution.Simple.Command
> import Distribution.Simple.Utils ( rawSystemExit )
> import Distribution.Simple -- (defaultMainWithHooks, autoconfUserHooks)
> import Distribution.Simple.InstallDirs  (CopyDest(..))
> import Distribution.Simple.LocalBuildInfo (absoluteInstallDirs, InstallDirs(..) )
> main = defaultMainWithHooks simpleUserHooks { postInst = myPostInst }

> buildRTS timberc dataDir
>  = do 
>       system("cd rtsPOSIX && chmod +x configure")
>       system("cd rtsPOSIX && ./configure --prefix=" ++ dataDir ++ " --with-timberc=" ++ timberc)
>       system("cd rtsPOSIX && make install")
>       return ()

> myPostInst args iflags pkg_descr lbi = do
>    let dirs = absoluteInstallDirs pkg_descr lbi NoCopyDest 
>        dataDir = datadir dirs
>        timberc = bindir dirs </> "timberc"
>        script = "#!/bin/sh\n\nexec " ++ dataDir ++ "/timberc ${1+\"$@\"} --datadir " ++ dataDir ++ "\n"
>    copyFile timberc (dataDir </> "timberc")
>    writeFile timberc script
>    buildRTS timberc dataDir
>    return ()

