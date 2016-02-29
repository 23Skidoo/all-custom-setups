#!/usr/bin/env runhaskell
> {-# LANGUAGE BangPatterns #-}

> import Distribution.Simple
> import Distribution.Simple.PreProcess
> import Distribution.Simple.Utils
> import qualified Tests.TestRunner as TR
> import Distribution.PackageDescription
> import Distribution.Simple.LocalBuildInfo
> import Data.Char
> import System.Exit
> import System.IO
> import System.Directory
> import System.FilePath.Windows

main = defaultMainWithHooks simpleUserHooks -- { runTests = TR.runPkgTestsHook }

> main = let hooks = simpleUserHooks 
>            xpp   = ("xpphs", ppXpp)
>            xhs   = ("xhs"  , ppX)
>        in defaultMainWithHooks hooks { hookedPreProcessors = xhs:xpp:knownSuffixHandlers  }
>
> ppXpp :: BuildInfo -> LocalBuildInfo -> PreProcessor 
> ppXpp build local =
>    PreProcessor {
>      platformIndependent = True,
>      runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
>        do info verbosity (inFile++" is being preprocessed to "++outFile)
>           let hscFile = replaceExtension inFile "hsc"
>           runSimplePreProcessor (ppCpp build local) inFile  hscFile verbosity
>           handle <- openFile hscFile ReadMode
>           source <- sGetContents handle
>           hClose handle
>           let newsource = unlines $ process $ lines source
>           writeFile hscFile newsource
>           runSimplePreProcessor (ppHsc2hs build local) hscFile outFile verbosity
>           removeFile hscFile
>           return ()
>      }
>
> ppX :: BuildInfo -> LocalBuildInfo -> PreProcessor 
> ppX build local =
>    PreProcessor {
>      platformIndependent = True,
>      runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
>        do info verbosity (inFile++" is being preprocessed to "++outFile)
>           handle <- openFile inFile ReadMode
>           source <- sGetContents handle
>           hClose handle
>           writeFile outFile source
>           return ()
>      }
> 
> process :: [String] -> [String]
> process ("":"":xs) = process ("":xs)
> process (x    :xs) = case x of
>                        ('#':str) -> let str' = dropWhile isSpace str
>                                         nm   = takeWhile isNumber str'
>                                     in if null nm
>                                           then x : process xs
>                                           else process xs
>                        _            -> x : process xs
> process []         = []
> 
> sGetContents h  = hGetContents h >>= \s -> length s `seq` return s