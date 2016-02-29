import Distribution.Simple
import Distribution.Simple.InstallDirs
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription

import System.Exit
import System.Process

-- After Epic is built, we need a run time system.

system' cmd = do 
    exit <- system cmd
    case exit of
      ExitSuccess -> return ()
      ExitFailure _ -> exitWith exit
-- FIXME: This is probably all done the wrong way, I don't really understand
-- Cabal properly...

buildLib args flags desc local 
    = system' "make -C evm"

-- This is a hack. I don't know how to tell cabal that a data file needs
-- installing but shouldn't be in the distribution. And it won't make the
-- distribution if it's not there, so instead I just delete
-- the file after configure.

postConfLib args flags desc local
    = system' "make -C evm clean"

addPrefix pfx var c = "export " ++ var ++ "=" ++ show pfx ++ "/" ++ c ++ ":$" ++ var

postInstLib args flags desc local
    = do let pfx = prefix (installDirTemplates local)
         system' $ "make -C evm install PREFIX=" ++ show pfx

main = defaultMainWithHooks (simpleUserHooks { postBuild = buildLib,
                                               postConf = postConfLib,
                                               postInst = postInstLib })

