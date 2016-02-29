
import Distribution.Simple
import Distribution.Simple.Utils
import Directory
import Monad
import System

neededHeaders = ["jack/jack.h", "st.h"]

main = do
    args <- getArgs
    when ((head args) == "configure") (checkHeaders neededHeaders)
    defaultMainWithHooks defaultUserHooks


checkHeaders ll = do
    mapM checkHeader ll
    return ()

includeDirs = ["/usr/local/include", "/usr/include"]

checkHeader name = do
    putStr ("Searching for " ++ name ++ "...")
    bools <- mapM (\ p -> doesFileExist (p ++ "/" ++ name)) includeDirs
    when (not $ or bools) (fail ("ERROR: " ++ name ++ " not found"))
    putStrLn "found"

