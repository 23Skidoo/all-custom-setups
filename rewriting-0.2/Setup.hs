module Main (main) where

import Distribution.Simple
import System.Cmd (system)
import System.FilePath ((</>))
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)

main :: IO ()
main = defaultMainWithHooks hooks where
  hooks = simpleUserHooks { runTests = runTests' }

runTests' _ _ _ _ = system cmd >> return ()
  where testdir = "dist" </> "build" </> "test"
        testcmd = "." </> "test"
        cmd = "cd " ++ testdir ++ " && " ++ testcmd

