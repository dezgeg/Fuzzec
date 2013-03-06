module Fuzzer.Combinators where
import Fuzzer.Core

oneOf :: [a] -> Fuzzer a
oneOf xs = do i <- fuzzerGetRandom $ randomR (0, length xs - 1)
              return $ xs !! i
