import Distribution.Simple
import Manatee.Core.Config
import Manatee.Core.Types
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Map
import Manatee.Extension.Browser.PageMode

import qualified Data.Map as M

main = defaultMainWithHooks 
       simpleUserHooks {
         -- Update Rule after install successful.
         postInst = \ _ _ _ _ -> do
             -- Update PageTypeRule.
             (PageTypeRule typeRule) <- readConfig pageTypeRulePath (PageTypeRule M.empty)
             writeConfig pageTypeRulePath (PageTypeRule (M.insert "PageBrowser" "manatee-browser" typeRule))
             -- Update FileOpenRule
             let match = ContentTypeMatch "text/html"
                 rule  = ("Open URI", "PageBrowser", "file://")
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
                                                (M.insert "PageBrowser" (Left $ pageModeName browserMode) modeRule))
             -- Update Duplicate tab list.
             (PageModeDuplicateList list) <- readConfig pageModeDuplicateList (PageModeDuplicateList [])
             writeConfig pageModeDuplicateList (PageModeDuplicateList (insertUnique (pageModeName browserMode) list))
       }
