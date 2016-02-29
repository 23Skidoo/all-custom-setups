{-# LANGUAGE Haskell2010
    , ScopedTypeVariables
 #-}
{-# OPTIONS
    -Wall
    -fno-warn-missing-signatures
    -fno-warn-name-shadowing
 #-}

import Prelude
import qualified Prelude as P

import Data.List

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess

import System.Environment
import System.FilePath

import Text.Printf

import Language.Preprocessor.Cpphs


main = do
    args <- getArgs

    let hooks = simpleUserHooks {
            hookedPreProcessors = [ ("hss", transformHss)
                                  , ("cpphs", transformCpphs) ]
          }

    defaultMainWithHooksArgs hooks args


transformCpphs :: BuildInfo -> LocalBuildInfo -> PreProcessor
transformCpphs _ _ = PreProcessor True $ \(inDir, inFile) (outDir, outFile) _ -> do
    source <- readFile (inDir </> inFile)

    _ <- printf "Preprocessing (CPPHS) %s -> %s\n"
        (inDir </> inFile) (outDir </> outFile)

    let options = defaultCpphsOptions {
            boolopts = (defaultBoolOptions { ansi = True })
          }

    runCpphs options (inDir </> inFile) source
        >>= writeFile (outDir </> outFile)


transformHss :: BuildInfo -> LocalBuildInfo -> PreProcessor
-- ^ Transforms a *.hss file containing TriString-literals ("""triString""")
-- into an ordinary *.hs file.
transformHss _ _ = PreProcessor True $ \(inDir, inFile) (outDir, outFile) _ -> do
    source <- readFile (inDir </> inFile)

    _ <- printf "Preprocessing (HSS) %s -> %s\n"
            (inDir </> inFile) (outDir </> outFile)

    writeFile (outDir </> outFile) (doProcessHss inFile source)


-- | The actual processing of 'transformHss'. No advanced method of parsing
-- have been used in order to reduce the dependencies of the Setup script
-- and to now blow it up by a whole combinator library or auto generated code.
--
-- Every triString is replaced by the concatenation of ordinary haskell
-- strings. Code embedded in curly braces (introduced by a hash sign) is
-- embedded literally. LINE pragmas keep the line numbers consistent.
doProcessHss file = processHss (1 :: Int)
  where
    processHss n xs = case xs of
        ('"':'"':'"':xs) -> triString n xs
        ('{':'-':xs) -> '{' : '-' : comment n (1 :: Int) xs
        ('"':xs) -> '"' : string n xs
        ('\n':xs) -> '\n' : processHss (succ n) xs
        (x:xs) -> x : processHss n xs
        "" -> ""
      where
        triString n xs = case breakIt xs of
            (str, xs) -> concat [ "((\\_ -> let { __ = "
                , "\n{-# LINE ", show n, " ", show file, " #-}\n"
                , sanitizeIt (mkTriString str)
                , "\n{-# LINE ", show n', " ", show file, " #-}\n"
                , "} in __) undefined)"
                , processHss n' xs ]
              where n' = n + length (filter (== '\n') str)

        breakIt xs = case xs of
            ('"':'"':'"':xs) -> ("", xs)
            (x:xs) -> case breakIt xs of (y, ys) -> (x : y, ys)
            "" -> ("", "")

        sanitizeIt xs = case xs of
            ('\\':'\\':'n':xs) -> sanitizeIt xs
            (x:xs) -> x : sanitizeIt xs
            "" -> ""

        mkTriString xs = "concat [\"" ++ process string ++ "\"]"
          where
            string = concat $ intersperse "\\n" lines
            process xs = case xs of
                ('#':'{':xs) -> case break (== '}') xs of
                    (ls, (_:rs)) -> "\", (" ++ ls ++ "), \"" ++ process rs
                    _ -> error "could not match closing curly brace"
                ('"':xs) -> '\\' : '"' : process xs
                (x:xs) -> x : process xs
                [] -> []
            lines = case P.lines xs of
                ("":lines) -> map (dedent (indent lines)) lines
                (line:[]) -> [line]
                lines@(_:lines') -> map (dedent (indent lines')) lines
                [] -> []
            indent xs = case xs of
                [] -> 0
                _ -> minimum $ map (length . takeWhile (== ' ')) xs
            dedent n line = case splitAt n line of
                (ls, rs) -> dropWhile (== ' ') ls ++ rs

        comment l 1 ('-':'}':xs) = '-' : '}' : processHss l xs
        comment l n xs = case xs of
            ('{':'-':xs) -> '{' : '-' : comment l (succ n) xs
            ('-':'}':xs) -> '-' : '}' : comment l (pred n) xs
            ('\n':xs) -> '\n' : comment (succ l) n xs
            (x:xs) -> x : comment l n xs
            "" -> ""

        string l xs = case xs of
            ('\\':'"':xs) -> '\\' : '"' : string l xs
            ('"':xs) -> '"' : processHss l xs
            (x:xs) -> x : string l xs
            "" -> ""


