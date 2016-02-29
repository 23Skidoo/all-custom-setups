import Distribution.Simple
import Manatee.Core.Config
import Manatee.Core.Types
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Map
import Manatee.Extension.PdfViewer.PageMode

import qualified Data.Map as M

main = defaultMainWithHooks 
       -- Update Rule after install successful.
       simpleUserHooks {
         postInst = \ _ _ _ _ -> do
             -- Update PageTypeRule.
             (PageTypeRule rule) <- readConfig pageTypeRulePath (PageTypeRule M.empty)
             writeConfig pageTypeRulePath (PageTypeRule (M.insert "PagePDF" "manatee-pdfviewer" rule))
             -- Update FileOpenRule.
             let match = ContentTypeMatch "application/pdf"
                 rule  = ("Open PDF", "PagePDF", "")
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
                                                (M.insert "PagePDF" (Left $ pageModeName pdfMode) modeRule))
       }
