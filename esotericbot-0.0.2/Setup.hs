import Distribution.Simple

import Distribution.Package
import Distribution.Simple.Program(defaultProgramConfiguration)
import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple.Command

import Distribution.Simple.Utils (rawSystemExit, cabalVersion)

import Distribution.License (License(..))
import Distribution.Version
         ( Version(..) )
import Distribution.Text
         ( display )

import System.Environment (getArgs, getProgName)
import Data.List  (intersperse)
import System.Exit

make = getArgs >>= makeHelper

makeHelper :: [String] -> IO ()
makeHelper args =
  case commandsRun globalCommand commands args of
    CommandHelp   help                 -> printHelp help
    CommandList   opts                 -> printOptionsList opts
    CommandErrors errs                 -> printErrors errs
    CommandReadyToGo (flags, commandParse)  ->
      case commandParse of
        _ | fromFlag (globalVersion flags)        -> printVersion
          | fromFlag (globalNumericVersion flags) -> printNumericVersion
        CommandHelp     help           -> printHelp help
        CommandList     opts           -> printOptionsList opts
        CommandErrors   errs           -> printErrors errs
        CommandReadyToGo action        -> action

  where
    printHelp help = getProgName >>= putStr . help
    printOptionsList = putStr . unlines
    printErrors errs = do
      putStr (concat (intersperse "\n" errs))
      exitWith (ExitFailure 1)
    printNumericVersion = putStrLn $ display cabalVersion
    printVersion        = putStrLn $ "Cabal library version "
                                  ++ display cabalVersion

    progs = defaultProgramConfiguration
    commands =
      [configureCommand progs `commandAddAction` configureAction
      ,buildCommand     progs `commandAddAction` buildAction
      ,installCommand         `commandAddAction` installAction
      ,copyCommand            `commandAddAction` copyAction
      ,cleanCommand           `commandAddAction` cleanAction
      ,registerCommand	      `commandAddAction` bugger_all
      ,unregisterCommand      `commandAddAction` bugger_all
      ,haddockCommand         `commandAddAction` bugger_all
      ]

bugger_all =
   const $ return $ return ( )

configureAction :: ConfigFlags -> [String] -> IO ()
configureAction flags args = do
  noExtraFlags args
  let verbosity = fromFlag (configVerbosity flags)
  rawSystemExit verbosity "sh" $
    "configure"
    : configureArgs backwardsCompatHack flags
  where backwardsCompatHack = True

copyAction :: CopyFlags -> [String] -> IO ()
copyAction flags args = do
  noExtraFlags args
  let destArgs = case fromFlag $ copyDest flags of
        NoCopyDest      -> ["install"]
        CopyTo path     -> ["copy", "destdir=" ++ path]
        CopyPrefix path -> ["install", "prefix=" ++ path]
                -- CopyPrefix is backwards compat, DEPRECATED
  rawSystemExit (fromFlag $ copyVerbosity flags) "make" destArgs

installAction :: InstallFlags -> [String] -> IO ()
installAction flags args = do
  noExtraFlags args
  rawSystemExit (fromFlag $ installVerbosity flags) "make" ["install"]

buildAction :: BuildFlags -> [String] -> IO ()
buildAction flags args = do
  noExtraFlags args
  rawSystemExit (fromFlag $ buildVerbosity flags) "make" []

cleanAction :: CleanFlags -> [String] -> IO ()
cleanAction flags args = do
  noExtraFlags args
  rawSystemExit (fromFlag $ cleanVerbosity flags) "make" ["clean"]

sdistAction :: SDistFlags -> [String] -> IO ()
sdistAction flags args = do
  noExtraFlags args
  rawSystemExit (fromFlag $ sDistVerbosity flags) "make" ["dist"]

main = do
   make 
   defaultMain
