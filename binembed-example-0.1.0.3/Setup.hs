import Distribution.Simple
import Distribution.Simple.BinEmbed

main :: IO ()
main = defaultMainWithHooks (withBinEmbed simpleUserHooks)
