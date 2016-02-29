{-# LANGUAGE CPP #-}

import           Control.Monad                      (foldM_, forM_)
import           Data.Maybe                         (fromMaybe)
import           System.Cmd
import           System.Directory                   (copyFile,
                                                     createDirectoryIfMissing,
                                                     doesFileExist, removeFile)
import           System.Exit
import           System.FilePath
import           System.Info                        (os)

import           Distribution.MacOSX
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
    { postInst = appBundleInstallHook [geniApp DoNotChase]
    , postBuild = appBundleBuildHook  [geniApp DoNotChase]
    }

geniApp :: ChaseDeps -> MacApp
geniApp =
  MacApp "geni-gui"
         (Just "macstuff/wxmac.icns")
         (Just "macstuff/Info.plist")
         []
         []
