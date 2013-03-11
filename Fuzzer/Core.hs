{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fuzzer.Core(
    Fuzzer,
    runFuzzer,
    module System.Random,
    module Control.Monad.Random.Class
) where

import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Random.Class
import System.Random

data FuzzerState = FuzzerState {

}
newtype Fuzzer a = Fuzzer { unFuzzer :: StateT FuzzerState (RandT StdGen IO) a }
    deriving (Monad, MonadState FuzzerState, MonadRandom)

runFuzzer :: Int -> Fuzzer a -> IO a
runFuzzer seed f = evalRandT (evalStateT (unFuzzer f) initialState) (mkStdGen seed)
    where initialState = FuzzerState { }
