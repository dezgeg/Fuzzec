module CPP.Chars where
import Fuzzer.Core
import Fuzzer.Combinators
import Control.Monad
import Control.Applicative

randomIdentifierChar = oneOf $ ['a'..'z'] ++ ['A'..'Z']

randomIdent :: Fuzzer String
randomIdent = replicateM 10 randomIdentifierChar
