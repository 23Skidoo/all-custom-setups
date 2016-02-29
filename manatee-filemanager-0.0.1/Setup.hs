import Distribution.Simple
import Manatee.Core.Config
import Manatee.Core.Types
import Manatee.Extension.FileManager.PageMode

import qualified Data.Map as M

main = defaultMainWithHooks 
       -- Update Rule after install successful.
       simpleUserHooks {
         postInst = \ _ _ _ _ -> do
             -- Update PageTypeRule.
             (PageTypeRule typeRule) <- readConfig pageTypeRulePath (PageTypeRule M.empty)
             writeConfig pageTypeRulePath (PageTypeRule (M.insert "PageFileManager" "manatee-filemanager" typeRule))
             -- Update PageModeRule.
             (PageModeRule modeRule) <- readConfig pageModeRulePath (PageModeRule M.empty)
             writeConfig pageModeRulePath (PageModeRule 
                                           (M.insert "PageFileManager" (Left $ pageModeName diredMode) modeRule))
       }
