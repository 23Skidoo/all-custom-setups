import Distribution.Simple
import Data.Digest.CRC16
import Test.HUnit.Text
import Tests

main = 
    defaultMainWithHooks ( simpleUserHooks { runTests = test_crc16 } )
    where
        test_crc16 _ _ _ _ = do runTestTT tests
                                return ()
