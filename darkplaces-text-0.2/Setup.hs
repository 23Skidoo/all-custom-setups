import Distribution.Simple
import Distribution.Simple.Setup
import Data.List (elem)


main = defaultMainWithHooks myhooks
  where
    hooks = simpleUserHooks
    myconf = wrapConfHook $ confHook hooks
    myhooks = hooks {confHook=myconf}


wrapConfHook f args conf = f args conf'
  where
    pargs = configProgramArgs conf
    alex_args = ("alex", ["--latin1"])
    conf' = if alex_args `elem` pargs
        then conf
        else conf {configProgramArgs=alex_args : pargs}
