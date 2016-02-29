#!/usr/bin/env runhaskell

import Distribution.Simple
main = do
  putStrLn msg
  defaultMainWithHooks simpleUserHooks

msg = "This links to the standard c library version of regular expressions.\n\
      \The corresponding c header file is regex.h and there is a chance you\n\
      \will need to edit the end of the regex-posix.cabal file to find the\n\
      \include directory and/or library.\n\
      \Alternatively you can try and use flags to the cabal executable to\n\
      \specify the include and lib directories."
