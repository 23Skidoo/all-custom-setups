import Distribution.PackageDescription      ( PackageDescription(..) )
import Distribution.Simple
import Distribution.Simple.InstallDirs      ( docdir, mandir, CopyDest (NoCopyDest) )
import Distribution.Simple.LocalBuildInfo   ( LocalBuildInfo(..), absoluteInstallDirs )
import Distribution.Simple.Program.Db       ( ProgramDb, lookupProgram )
import Distribution.Simple.Program.Run      ( runProgramInvocation, programInvocation, progInvokeCwd )
import Distribution.Simple.Program.Types    ( ConfiguredProgram, simpleProgram )
import Distribution.Simple.Setup            ( copyDest, copyVerbosity, fromFlag, installVerbosity, haddockVerbosity )
import Distribution.Simple.Utils            ( installOrdinaryFile, installOrdinaryFiles, notice )
import Distribution.Verbosity               ( Verbosity, moreVerbose )
import System.Exit                          ( exitSuccess )
import System.FilePath                      ( splitDirectories, joinPath, takeExtension, replaceExtension, (</>) )
import System.Directory                     ( getCurrentDirectory, setCurrentDirectory, createDirectoryIfMissing, doesFileExist )

main :: IO ()
main = do
  defaultMainWithHooks $ simpleUserHooks
    { postCopy = \ _ flags pkg lbi ->
         installManpages pkg lbi (fromFlag $ copyVerbosity flags) (fromFlag $ copyDest flags)

    , postInst = \ _ flags pkg lbi ->
         installManpages pkg lbi (fromFlag $ installVerbosity flags) NoCopyDest

    , postHaddock = \ _ flags pkg lbi ->
         runPdflatex pkg lbi (fromFlag $ haddockVerbosity flags)

    , hookedPrograms = [ simpleProgram "pdflatex" ]
    }
  exitSuccess

installManpages :: PackageDescription -> LocalBuildInfo -> Verbosity -> CopyDest -> IO ()
installManpages pkg lbi verbosity copy = do
    installOrdinaryFiles verbosity (mandir (absoluteInstallDirs pkg lbi copy))
        [ ("man", joinPath mp) | ("man":mp) <- map splitDirectories $ extraSrcFiles pkg ]

    installOrdinaryFiles' verbosity (docdir (absoluteInstallDirs pkg lbi copy))
            [ (buildDir lbi </> "latex", replaceExtension (last p) "pdf")
            | ("doc":p@(_:_)) <- map splitDirectories $ extraSrcFiles pkg
            , takeExtension (last p) == ".tex" ]

installOrdinaryFiles' :: Verbosity -> FilePath -> [(FilePath, FilePath)] -> IO ()
installOrdinaryFiles' verb dest = mapM_ (uncurry go)
  where
    go base src = do e <- doesFileExist (base </> src)
                     if e then installOrdinaryFile verb (base </> src) (dest </> src)
                          else notice verb $ show (base </> src) ++ " was not built, can't install."

withLatex :: LocalBuildInfo -> (ConfiguredProgram -> IO ()) -> IO ()
withLatex lbi k = maybe (return ()) k $ lookupProgram (simpleProgram "pdflatex") $ withPrograms lbi

runPdflatex :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO ()
runPdflatex pkg lbi verb =
    withLatex lbi $ \cmd -> do
        cwd <- getCurrentDirectory
        createDirectoryIfMissing True (buildDir lbi </> "latex")
        sequence_ [ runProgramInvocation (moreVerbose verb) $
                        (programInvocation cmd [ "-interaction=nonstopmode", cwd </> joinPath ("doc":f) ])
                        { progInvokeCwd = Just (buildDir lbi </> "latex") }
                  | ("doc":f@(_:_)) <- map splitDirectories $ extraSrcFiles pkg
                  , takeExtension (last f) == ".tex" ]

