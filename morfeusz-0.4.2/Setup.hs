import Control.Monad (forM_)
import System.FilePath
import System.Directory
import System.Info (os)
import Distribution.Simple
import Distribution.Simple.Setup
import qualified Distribution.Simple.Configure as C

import Setup.Configure

morfPath :: String
morfPath = "libmorfeusz"

morfLibSrc :: String
morfLibSrc = "libmorfeusz.so"

morfLibs :: [String]
morfLibs = ["libmorfeusz.so.0", "libmorfeusz.so"]

-- | We need to copy the libmorfeusz.so file before the configuration phase.
conf desc cfg = do
    if (null (configExtraLibDirs cfg) && os == "linux")
        then do
            path <- sharedPath desc cfg
            createDirectoryIfMissing True path
            forM_ morfLibs $ \lib -> do
                copyFile (morfPath </> morfLibSrc) (path </> lib)
            C.configure desc $ cfg { configExtraLibDirs = [path] }
        else do
            C.configure desc cfg

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { confHook = conf }
