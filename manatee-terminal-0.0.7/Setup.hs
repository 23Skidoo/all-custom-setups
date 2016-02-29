-- Author:     Andy Stewart <lazycat.manatee@gmail.com>
-- Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
-- 
-- Copyright (C) 2010 Andy Stewart, all rights reserved.
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

import Distribution.Simple
import Manatee.Core.Config
import Manatee.Core.Types
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Map
import Manatee.Extension.Terminal.PageMode

import qualified Data.Map as M

main = defaultMainWithHooks 
       simpleUserHooks {
         -- Update Rule after install successful.
         postInst = \ _ _ _ _ -> do
             -- Update PageTypeRule.
             (PageTypeRule typeRule) <- readConfig pageTypeRulePath (PageTypeRule M.empty)
             writeConfig pageTypeRulePath (PageTypeRule (M.insert "PageTerminal" "manatee-terminal" typeRule))
             -- Update PageModeRule.
             (PageModeRule modeRule) <- readConfig pageModeRulePath (PageModeRule M.empty)
             writeConfig pageModeRulePath (PageModeRule 
                                                (M.insert "PageTerminal" (Left $ pageModeName terminalMode) modeRule))
             -- Update ExtensionGloalKeymap.
             (ExtensionGloalKeymap keymap) <- readConfig extensionGlobalKeymapPath (ExtensionGloalKeymap M.empty)
             writeConfig extensionGlobalKeymapPath (ExtensionGloalKeymap
                                                    (M.insert "P-n" ("PageTerminal", "Terminal", []) keymap))
       }
