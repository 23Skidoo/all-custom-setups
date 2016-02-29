#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.PreProcess
import BuildTools.Template2Hs

main = defaultMainWithHooks myHooks
    where myHooks           = simpleUserHooks { hookedPreProcessors = [("msg", ppMsgTemplate)]}
          ppMsgTemplate _ _ = PreProcessor { platformIndependent = True,
                                             runPreProcessor = mkSimplePreProcessor template2hs }
