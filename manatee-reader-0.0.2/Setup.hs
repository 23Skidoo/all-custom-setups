import Distribution.Simple
import Manatee.Core.Config
import Manatee.Core.Types
import Manatee.Extension.Reader.PageMode

import qualified Data.Map as M

main = defaultMainWithHooks 
       simpleUserHooks {
         -- Update PageTypeRule after install successful.
         postInst = \ _ _ _ _ -> do
             (PageTypeRule rule) <- readConfig pageTypeRulePath (PageTypeRule M.empty)
             writeConfig pageTypeRulePath (PageTypeRule (M.insert "PageReader" "manatee-reader" rule))
             -- Update PageModeRule.
             (PageModeRule modeRule) <- readConfig pageModeRulePath (PageModeRule M.empty)
             writeConfig pageModeRulePath (PageModeRule 
                                                (M.insert "PageReader" (Left $ pageModeName readerMode) modeRule))
       }
