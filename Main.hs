module Main where
import Fuzzer.Core
import Control.Applicative
import CPP.Chars

main = do ident <- runFuzzer 42 randomIdent
          print ident
