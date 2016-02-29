#! /usr/bin/env runhaskell
>
> import Control.Monad
> import Distribution.Simple
> import Distribution.PackageDescription
> import System.IO
> import System.Directory
>
> copyFileWithPrefix src tgt prefix = 
>   readFile src >>= \txt -> writeFile tgt (prefix ++ txt)
>
> copyFiles srcdir tgtdir prefix = do
>   files <- getDirectoryContents srcdir
>   forM_ files $ \fname -> do
>     let src = srcdir ++ fname
>         tgt = tgtdir ++ fname
>     doesFileExist src >>= \b -> when b $ copyFileWithPrefix src tgt prefix
>     doesDirectoryExist src>>= \b -> when ( b && fname /= "." && fname /= ".." ) $ do
>       createDirectoryIfMissing False tgt
>       copyFiles (src ++ "/") (tgt ++ "/") prefix
>
> -- thePrefix flt = "{-# OPTIONS_GHC -DFlt=" ++ flt ++ " -DVECT_" ++ flt ++ " #-}\n"
> thePrefix flt = unlines
>   [ "{-# LANGUAGE CPP #-}"
>   , "#define Flt " ++ flt 
>   , "#define VECT_" ++ flt
>   ]
>
> myPreBuildHook args buildflags = do
>   createDirectoryIfMissing False "Data/Vect/Float"
>   createDirectoryIfMissing False "Data/Vect/Double"
>   copyFileWithPrefix "src/flt.hs" "Data/Vect/Float.hs"  (thePrefix "Float")
>   copyFileWithPrefix "src/flt.hs" "Data/Vect/Double.hs" (thePrefix "Double")
>   copyFiles "src/flt/" "Data/Vect/Float/"  (thePrefix "Float")
>   copyFiles "src/flt/" "Data/Vect/Double/" (thePrefix "Double")
>   return $ emptyHookedBuildInfo  
>
> myPostCleanHook args cleanflags pdep mlocalbuildinfo = do
>   removeDirectoryRecursive "Data/Vect/Float"
>   removeDirectoryRecursive "Data/Vect/Double"
>
> myUserHooks = simpleUserHooks 
>   { preBuild = myPreBuildHook 
>   , postClean = myPostCleanHook
>   }
>
> main = do
>   defaultMainWithHooks myUserHooks
>