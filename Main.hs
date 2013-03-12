module Main(main) where
import Fuzzer.Core
import Control.Applicative
import CPP.Chars

main = do print =<< runFuzzer 42 randomIdent
          print =<< runFuzzer 41 randomPunctuation
          print =<< runFuzzer 41 randomString
