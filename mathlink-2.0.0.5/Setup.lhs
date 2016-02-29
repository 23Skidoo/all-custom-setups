#!/usr/bin/env runhaskell
\begin{code}

import Distribution.Simple
import Distribution.System
import System.IO
import System.Directory

buildInfoFileName = "mathlink.buildinfo"
archHeaderFileName = "cbits/arch.h"

fileNames = [ buildInfoFileName
            , archHeaderFileName
            ]

bits = if is64bit then 64 else 32
  where Platform arch _ = buildPlatform
        is64bit = case arch of
                    X86_64 -> True
                    IA64   -> True
                    PPC64  -> True
                    _      -> False

buildInfoFile = "\
\Extra-Libraries: ML" ++ show bits ++ "i3, rt\n"

archHeaderFile = "\
\#ifndef __ARCH_H__\n\
\#define __ARCH_H__\n\
\\n\
\#define IS_" ++ show bits ++ "_BIT\n\
\\n\
\#endif\n"

removeIfExists fn = do
  bl <- doesFileExist fn 
  if bl then removeFile fn else return ()

makeFiles _ _ _ _ = do
  writeFile buildInfoFileName buildInfoFile
  writeFile archHeaderFileName archHeaderFile

removeFiles _ _ _ _ = mapM_ removeIfExists fileNames

mathLinkHooks = autoconfUserHooks {
                  postConf = makeFiles
                , postClean = removeFiles
                }

main = defaultMainWithHooks mathLinkHooks

\end{code}
