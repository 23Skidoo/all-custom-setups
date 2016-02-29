module Main where

import System.FilePath

import Distribution.Simple
import Distribution.Simple.Setup as SS
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.PackageDescription 
import Distribution.Verbosity

import Curry.Files.CymakePath

kics :: Program
kics = simpleProgram "kics"

cymake :: IO Program
cymake = do
  call <- getCymake
  return (simpleProgram (takeBaseName call))
            {programFindLocation = \_-> return (Just call)}

main :: IO ()
main = do
  cymakeProg <- cymake
  defaultMainWithHooks simpleUserHooks
    {hookedPrograms=kics:cymakeProg:hookedPrograms simpleUserHooks
    ,postConf=myPostConf
    ,hookedPreProcessors=[("curry",mkModule)]
    }

unflag = fromFlagOrDefault silent

requireProg :: SS.Flag Verbosity -> LocalBuildInfo -> IO Program -> IO ()
requireProg verb lbi prog = do
  p <- prog
  requireProgram (unflag verb) p AnyVersion (withPrograms lbi)
  return ()
 
callProg :: Verbosity -> LocalBuildInfo -> IO Program -> Args -> IO ()
callProg verb lbi prog args = do
  p <- prog
  rawSystemProgramConf verb p (withPrograms lbi) args
  
myPostConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostConf args confFlags descrs lbi = do
  let require = requireProg (configVerbosity confFlags) lbi
  require cymake
  postConf simpleUserHooks args confFlags descrs lbi

mkModule :: BuildInfo -> LocalBuildInfo -> PreProcessor
mkModule buildInfo lbi = PreProcessor
  {platformIndependent = True
  ,runPreProcessor     = mkSimplePreProcessor (runKics lbi)}

runKics :: LocalBuildInfo -> FilePath -> FilePath -> Verbosity -> IO ()
runKics lbi infile outfile verb = do
  let call    = callProg verb lbi
      datadir = takeDirectory infile
   -- nomake not supported
  mapM (\ format -> call cymake ["--no-hidden-subdir",
                                 "-e",
                                 '-':'-':format,
                                 "-i"++datadir,
                                 infile])
       ["flat","extended-flat","acy","uacy","parse-only"] 
  call (return kics) ["-nomake","-o",outfile,infile]



