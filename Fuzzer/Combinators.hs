module Fuzzer.Combinators where
import Fuzzer.Core

oneOf :: [a] -> Fuzzer a
oneOf xs = do i <- getRandomR (0, length xs - 1)
              return $ xs !! i
