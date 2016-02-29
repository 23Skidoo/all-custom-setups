#!/usr/bin/env runghc
> import Data.List
> import Distribution.Simple
> import System.IO (hGetContents)
> import System.Process (waitForProcess, runInteractiveCommand)
> 
> main :: IO ()
> main = writeBuildInfo >> defaultMainWithHooks defaultUserHooks
> 
> writeBuildInfo :: IO ()
> writeBuildInfo = do
>    (_,out,_,pid) <- runInteractiveCommand "sdl-config --prefix --libs --cflags"
>    res <- hGetContents out
>    length res `seq` waitForProcess pid
>    case lines res of
>        (prefix@(_:_):libs@(_:_):cflags@(_:_):_) -> do
>            let x `isSubStringOf` s = or [ x `isPrefixOf` t | t <- tails s ]
>            writeFile "TimePiece.buildinfo" $ unlines
>                [ "Include-Dirs: " ++ prefix ++ "/include"
>                , "Extra-Lib-Dirs: " ++ prefix ++ "/lib"
>                , "CC-Options: " ++ cflags
>                , "GHC-Options: " ++ unwords (map ("-optc"++) $ words cflags)
>                ] ++ if "SDLmain" `isSubStringOf` libs
>                    then "Extra-Libraries: SDLmain\n"
>                    else ""
>        _  -> return ()
