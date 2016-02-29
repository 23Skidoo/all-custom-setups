import qualified Config.Simple as FortranizedSimple
import Config.Program

import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { hookedPrograms = gfortranProgram : hookedPrograms simpleUserHooks,
    confHook       = myConfHook,
    buildHook      = FortranizedSimple.defaultBuildHook }

myConfHook (pkg0, pbi) flags = do
    lbi <- confHook simpleUserHooks (pkg0, pbi) flags
    return lbi
