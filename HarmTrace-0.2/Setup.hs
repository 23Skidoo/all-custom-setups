module Main (main) where

import Distribution.Simple
import System.FilePath ((</>))
import System.Cmd (system)
import System.IO (hFlush, stdout)
import System.Exit
import System.Directory

main :: IO ()
main = defaultMainWithHooks (simpleUserHooks  { postConf = mypostConf })

mypostConf a f d b = let path = "MIR" </> "GeneratedInstances"
                         file = "GenerateInstances.hs"
                         ghc = "ghc "
                         flags = "-e main "
                     in do putStr "Generating instances..." 
                           hFlush stdout
                           setCurrentDirectory path
                           ec <- system (ghc ++ flags ++ file)
                           case ec of
                             ExitSuccess   -> putStrLn " done."
                             ExitFailure _ -> putStrLn " failed!" >> exitWith ec
                           postConf simpleUserHooks a f d b

