--
-- HTTP client for use with io-streams
--
-- Copyright © 2013 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the BSD licence.
--

import Data.Char (toUpper)
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Setup (ConfigFlags)
import Distribution.System (OS (..), buildOS)
import System.IO (IOMode (..), hPutStrLn, withFile)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
       postConf = configure
    }

{-
    Simple detection of which operating system we're building on;
    there's no need to link the Cabal logic into our library, so
    we'll keep using CPP in Network.Http.Inconvenience.
-}

configure :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
configure _ _ _ _  = do

    withFile "config.h" WriteMode (\h -> do
        hPutStrLn h ("#define " ++ s))

    return ()

  where
    o = buildOS

    s = case o of
            Linux   -> "__LINUX__"
            OSX     -> "__MACOSX__"
            Windows -> "__WINDOWS__"
            _       -> "__" ++ up o ++ "__"

    up x = map toUpper (show x)
