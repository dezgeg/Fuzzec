{-# LANGUAGE NoMonomorphismRestriction #-}
module Fuzzer.Combinators(
    between,
    choice,
    count,
    oneOf,
    sepBy,
    endBy
) where

import Control.Applicative
import Control.Monad
import Fuzzer.Core
import Data.List

choice :: [Fuzzer a] -> Fuzzer a
choice [] = fail "Empty list argument to choice"
choice xs = do i <- getRandomR (0, length xs - 1)
               xs !! i

count n fz | n < 0 = []
           | otherwise = replicateM n fz

-- TODO: add evilness
between c1 c2 fz = concat <$> sequence [c1, fz, c2]

oneOf :: [a] -> Fuzzer a
oneOf [] = fail "Empty list argument to oneOf"
oneOf xs = do i <- getRandomR (0, length xs - 1)
              return $ xs !! i

sepBy = liftA2 intercalate
endBy sep = concatMap (++[sep])
