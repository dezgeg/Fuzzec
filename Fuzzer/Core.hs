{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
module Fuzzer.Core(
    Fuzzer,
    runFuzzer,
    module System.Random,
    module Control.Monad.Random.Class,
    chance
) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Random.Class
import System.Random

data FuzzerState = FuzzerState {

}
newtype Fuzzer a = Fuzzer { unFuzzer :: StateT FuzzerState (RandT StdGen IO) a }
    deriving (Functor, Applicative, Monad, MonadState FuzzerState, MonadRandom)

runFuzzer :: Int -> Fuzzer a -> IO a
runFuzzer seed f = evalRandT (evalStateT (unFuzzer f) initialState) (mkStdGen seed)
    where initialState = FuzzerState { }

randomFloat = getRandomR (0.0, 1.0)

chance :: Float -> Fuzzer a -> Fuzzer a -> Fuzzer a
chance p c1 c2 = randomFloat >>= \flt -> if flt < p then c1 else c2
