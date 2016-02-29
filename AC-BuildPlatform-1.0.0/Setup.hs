module Main where

import Prelude hiding (catch)
import Distribution.Simple
import Control.Exception (catch, IOException)
import System.IO
import System.Directory
import System.Process
import System.Exit (ExitCode (..))

main = defaultMainWithHooks hooks

hooks = simpleUserHooks {postConf = gather}

gather args cfg pkg bld = do
  putStrLn "-| Gathering platform data..."

  putStrLn "---| Attempting to invoke Setup.hs compiled program..."
  let cmd0 = proc "./dist/setup/setup" ["+RTS", "--info"]
  let cmd1 = cmd0 {std_out = CreatePipe}

  catch
    (do
      (Nothing, Just hout, Nothing, pid) <- createProcess cmd1
      txt  <- hGetContents hout
      exit <- waitForProcess pid

      case exit of
        ExitFailure n -> do
          putStrLn $ "---| Compiled Setup.hs program failed with return code " ++ show n
          save Nothing Nothing Nothing
        ExitSuccess   -> do
          (os, cn, cv) <- parse txt
          save os cn cv
    )
    (\ e -> do
      let f = e `asTypeOf` (undefined :: IOException)
      putStrLn "---| Could not run."
      save Nothing Nothing Nothing
    )

parse :: String -> IO (Maybe String, Maybe String, Maybe String)
parse txt = do
  putStrLn "---| OK. Parsing program result..."
  case txt of
    "" -> do
      putStrLn "---| Empty result."
      return (Nothing, Nothing, Nothing)
    _  ->
      case readsPrec 0 txt of
        [] -> do
          putStrLn "---| Parser error."
          return (Nothing, Nothing, Nothing)
        (info, _):_ ->
          let
            os   =
              case lookup "Target OS" info of
                Nothing        -> Nothing
                Just "mingw32" -> Just "MS_Windows"
                Just _         -> Just "Unix"
            (name, ver) =
              case lookup "GHC version" info of
                Nothing -> (Nothing   , Nothing)
                Just v  -> (Just "GHC", Just v )
          in return (os, name, ver)

save :: Maybe String -> Maybe String -> Maybe String -> IO ()
save os cn cv = do
  putStrLn $ "-----| Operating System = " ++ show os
  putStrLn $ "-----| Compiler name    = " ++ show cn
  putStrLn $ "-----| Compiler version = " ++ show cv

  x <- doesDirectoryExist "Include"
  if x
    then return ()
    else do
      putStrLn "-| Create Include folder..."
      createDirectory "Include"

  putStrLn $ "-| Saving results..."
  writeFile "Include/Platform.h" $ unlines $
    [
      "#define HS_PNAME " ++ (case os of Nothing -> "Nothing"; Just t -> "Just " ++ t),
      "#define HS_CNAME " ++ show cn,
      "#define HS_CVERS " ++ show cv
    ]
  putStrLn $ "-| Done."
