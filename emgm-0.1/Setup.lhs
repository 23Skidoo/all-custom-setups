#! /usr/bin/env runhaskell

\begin{code}

{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Main (main) where

import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, UserHooks(..))
import System.Cmd (system)
import System.FilePath ((</>))

main :: IO ()
main = defaultMainWithHooks hooks where
  hooks = simpleUserHooks { runTests = runTests' }

runTests' _ _ _ _ = system cmd >> return ()
  where testdir = "dist" </> "build" </> "test"
        testcmd = "." </> "test"
        cmd = "cd " ++ testdir ++ " && " ++ testcmd

\end{code}

