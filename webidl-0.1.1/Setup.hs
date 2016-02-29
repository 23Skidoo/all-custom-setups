module Main (main) where

import Control.Monad
import System.Process
import System.Exit
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
  preConf = buildLexer
}

buildLexer :: Args -> ConfigFlags -> IO HookedBuildInfo

buildLexer args cflgs = do
  let cmd = "cd cbits; make lexer-pkg"
  runCommand cmd >>= waitForProcess
  return emptyHookedBuildInfo

