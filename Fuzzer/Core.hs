{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fuzzer.Core(
    Fuzzer,
    runFuzzer,
    fuzzerGetRandom,
    module System.Random
) where

import Control.Monad.State
import System.Random

data FuzzerState = FuzzerState {
    randomGen :: StdGen
}
newtype Fuzzer a = Fuzzer { unFuzzer :: StateT FuzzerState IO a } 
    deriving (Monad, MonadState FuzzerState)

runFuzzer :: Int -> Fuzzer a -> IO a
runFuzzer seed f = evalStateT (unFuzzer f) initialState
    where initialState = FuzzerState { randomGen = mkStdGen seed }

fuzzerGetRandom f = do
    s0 <- gets randomGen
    let (rv, s1) = f s0
    modify (\st -> st { randomGen = s1 })
    return rv
