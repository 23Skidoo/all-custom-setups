import Distribution.Simple
import Distribution.Simple.Program

trhsxProgram = simpleProgram "trhsx"

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
         hookedPrograms = [trhsxProgram]
       }
