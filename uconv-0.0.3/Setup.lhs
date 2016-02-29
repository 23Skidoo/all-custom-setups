#!/usr/bin/env runghc
> import Distribution.Simple
> import System.IO (hGetContents)
> import System.Process (waitForProcess, runInteractiveCommand)
> 
> main :: IO ()
> main = writeBuildInfo >> defaultMainWithHooks defaultUserHooks
> 
> writeBuildInfo :: IO ()
> writeBuildInfo = do
>    (_,out,_,pid) <- runInteractiveCommand "icu-config --prefix"
>    res <- hGetContents out
>    length res `seq` waitForProcess pid
>    case lines res of
>        (prefix@(_:_):_) -> do
>            writeFile "uconv.buildinfo" $ unlines
>                [ "Include-Dirs: " ++ prefix ++ "/include"
>                , "Extra-Lib-Dirs: " ++ prefix ++ "/lib"
>                ]
>        _  -> return ()
