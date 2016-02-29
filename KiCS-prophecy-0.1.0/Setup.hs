module Main where

import System.FilePath
import System.Directory
import Monad
import Data.List

import Distribution.Simple
import Distribution.Simple.Setup 
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.PackageDescription 
import Distribution.Verbosity
import Distribution.ModuleName (ModuleName(..))

import Curry.Files.CymakePath
import Curry.Files.KiCSPath

kics, cymake :: IO Program
kics   = mkProg getKiCS
cymake = mkProg getCymake

mkProg getProg = do
  call <- getProg
  return (simpleProgram (takeBaseName call))
            {programFindLocation = \_-> return (Just call)}

prophecy, mkstrict :: Program
prophecy = simpleProgram "prophecy"
mkstrict = simpleProgram "mkstrict"

main :: IO ()
main = do
  cymakeProg <- cymake
  kicsProg   <- kics
  defaultMainWithHooks simpleUserHooks
    {hookedPrograms=kicsProg:cymakeProg:hookedPrograms simpleUserHooks
    ,postConf=myPostConf
    ,hookedPreProcessors=[("curry",mkModule)]
    }

unflag = fromFlagOrDefault silent

requireProg :: ConfigFlags -> LocalBuildInfo -> IO Program 
            -> IO ConfiguredProgram
requireProg verb lbi prog = do
  p <- prog
  (cp,_) <- requireProgram (unflag $ configVerbosity verb) p AnyVersion 
                           (withPrograms lbi)
  return cp
 
callProg :: Verbosity -> LocalBuildInfo -> IO Program -> Args -> IO ()
callProg verb lbi prog args = do
  p <- prog
  rawSystemProgramConf verb p (withPrograms lbi) args
  
myPostConf :: 
  Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostConf args confFlags descrs lbi = do
  let require = requireProg confFlags lbi
  require cymake
  require kics
  postConf simpleUserHooks args confFlags descrs lbi

mkModule :: BuildInfo -> LocalBuildInfo -> PreProcessor
mkModule _ lbi = PreProcessor
  {platformIndependent = True
  ,runPreProcessor     = mkSimplePreProcessor $ \ inf outf verb -> do
                          datadir <- getKiCSLibDir 
                          runKics datadir lbi inf outf verb}

runKics :: FilePath -> LocalBuildInfo 
        -> FilePath -> FilePath -> Verbosity -> IO ()
runKics datadir lbi infile outfile verb 
  = do call cymake ["-iCurry/Module","-i"++datadir,
                    "--no-hidden-subdir",
                    "-e","--flat",infile]
       callKics   $ ["-userlibpath","Curry/Module"] ++
         if isSuffixOf "Transform.curry" infile 
         then ["-executable","prophecy"]
         else []
 where
    call = callProg verb lbi
    callKics args = 
      call kics $ args ++ ["-nomake","-o",outfile,infile]

