import Distribution.Simple
import Manatee.Core.Config
import Manatee.Core.Types
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Map
import Manatee.Extension.ImageViewer.PageMode

import qualified Data.Map as M

main = defaultMainWithHooks 
       simpleUserHooks {
         -- Update PageTypeRule after install successful.
         postInst = \ _ _ _ _ -> do
             (PageTypeRule rule) <- readConfig pageTypeRulePath (PageTypeRule M.empty)
             writeConfig pageTypeRulePath (PageTypeRule (M.insert "PageImage" "manatee-imageviewer" rule))
             -- Update FileOpenRule.
             let match = RegexpMatch "^image/*"
                 rule  = ("Open image", "PageImage", "")
             (FileOpenRule ruleMap) <- readConfig fileOpenRulePath (FileOpenRule M.empty)
             writeConfig fileOpenRulePath 
                         (FileOpenRule (let openRules = 
                                                case findMinMatch ruleMap (\ oMath _ -> oMath == match) of
                                                  Just (_, rules) -> insertUnique rule rules
                                                  Nothing -> [rule]
                                        in M.insert match openRules ruleMap))
             -- Update PageModeRule.
             (PageModeRule modeRule) <- readConfig pageModeRulePath (PageModeRule M.empty)
             writeConfig pageModeRulePath (PageModeRule 
                                                (M.insert "PageImage" (Left $ pageModeName imageMode) modeRule))
       }
