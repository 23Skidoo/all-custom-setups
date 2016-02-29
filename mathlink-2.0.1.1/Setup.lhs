#!/usr/bin/env runhaskell
\begin{code}

import Distribution.Simple
import Distribution.System
import System.IO
import System.Directory
import Data.List

-- Make sure that 'bits' and 'extraLibs' below is correctly defined
-- for your platform. See Distribution.System for the definitions of
-- the relevant architecture/OS enumerations.

-- string representing the word size of your platform
bits = case buildPlatform of
         Platform X86_64 _ -> "64"
         Platform IA64   _ -> "64"
         Platform PPC64  _ -> "64"
         _                 -> "32"

-- list of extra libraries against which you need to link
extraLibs = intercalate ", " libs
  where libs = case buildPlatform of
                 Platform _    Linux   -> ["rt"]

                 -- If this setting for windows isn't right, let me know.
                 -- I haven't tried it, bu just guessed what it should be 
                 -- from the definition of the mcc script.
                 Platform I386 Windows -> ["Gdi32"]

                 -- For now, nothing extra is specified for other platforms.
                 -- If you are on another platform and you needed to 
                 -- link against extra libraries, let me know what they
                 -- are.
                 _                     -> []


buildInfoFileName = "mathlink.buildinfo"
archHeaderFileName = "cbits/arch.h"

buildInfoFile = "\
\Extra-Libraries: ML" ++ bits ++ "i3, " ++ extraLibs ++ "\n"

archHeaderFile = "\
\#ifndef __ARCH_H__\n\
\#define __ARCH_H__\n\
\\n\
\#define IS_" ++ bits ++ "_BIT\n\
\\n\
\#endif\n"

makeFiles _ _ _ _ = do
  writeFile buildInfoFileName buildInfoFile
  writeFile archHeaderFileName archHeaderFile

main = defaultMainWithHooks autoconfUserHooks {
         postConf = makeFiles
       }

\end{code}
