module Main where

import Distribution.Simple
import System.Directory

import Configure.Detect (detect)
import Configure.Write  (write)

main = defaultMainWithHooks hooks

hooks = simpleUserHooks {postConf = check}

check _ _ _ _ = do
  putStrLn "-| Checking for platform values..."
  x1 <- doesDirectoryExist "Include"
  if x1 then return () else createDirectory "Include"
  x2 <- doesFileExist "Include/Platform.h"
  if x2
    then putStrLn "-| Platform values have already been set."
    else do
      putStrLn "-| Platform values are not yet set. Attempting to guess them..."
      (m_os, m_cname, m_cver) <- detect
      case m_os of
        Nothing -> putStrLn "-| FATAL: Cannot detect OS type."
        Just os -> write (os, maybe "" id m_cname, maybe "" id m_cver)
