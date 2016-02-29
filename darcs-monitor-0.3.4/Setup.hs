{-  darcs-monitor - Darcs repository monitor
    Copyright © 2007, 2008 Antti-Juhani Kaijanaho

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-}
import Control.Monad
import Data.Version
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Directory
import System.Exit
import System.IO.Error
import System.Time

main = defaultMainWithHooks defaultUserHooks { confHook = conf
                                             , postClean = pclean
                                             , postBuild = pbuild
                                               }

conf gpd cf = do
  let PackageDescription { package = PackageIdentifier 
                                     { pkgVersion = ver
                                     , pkgName = name 
                                     }
                         }
          = case gpd of
              (Left GenericPackageDescription { packageDescription = pd }, _) 
                  -> pd
              (Right pd, _)
                  -> pd
  writeFile "Version.hs" $ "-- GENERATED FILE -- DO NOT EDIT\n\
                           \module Version where\n\
                           \packageVersion = \"" ++
                           name ++ " " ++
                           showVersion ver ++ "\""
  configure gpd cf

substFiles = ["darcs-monitor.1"]

pclean _ _ _ _ = do flip catch ct $ do
                      removeDirectory "dist"
                      removeFile "Version.hs"
                      mapM_ removeFile substFiles
    where ct ioe | isDoesNotExistError ioe = return ()
                 | otherwise               = ioError ioe

data Repl = Repl { replDataDir :: FilePath }

pbuild _ _ pd lbi = do
  mapM_ (substFile pd lbi) substFiles

substFile pd lbi outf = do
  let inf = outf ++ ".in"
  imd <- getModificationTime inf
  omd <- catch (getModificationTime outf) $ \ioe ->
         if isDoesNotExistError ioe
         then return (TOD 0 0)
         else ioError ioe
  when (imd > omd) $ do
    putStrLn ("Generating " ++ outf ++ " ...")
    is <- readFile inf
    let f ('@':'D':'A':'T':'A':'D':'I':'R':'@':r) = 
            (datadir (absoluteInstallDirs pd lbi NoCopyDest)) ++ f r
        f (c:r) = c : f r
        f []    = []
    writeFile outf (f is)

