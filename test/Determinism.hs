module Main where

import Data.Word
import SoftFloat
import System.Exit
import Control.Monad
import Control.Concurrent

-- | Generate a bunch of floats
forkA :: IO (Chan Word64)
forkA = do
  chan <- newChan
  let u = ui64ToF64 RoundNearEven 1000000000
  forkIO $ let go x = do
                    let x' = f64Add RoundNearEven x x
                    writeChan chan (f64Div RoundNearEven one zero)
                    writeChan chan (f64Add RoundNearMaxMag x' (f64Div RoundNearEven one zero))
                    writeChan chan (f64Mul RoundNearMaxMag x' (f64Div RoundNearEven one zero))
                    writeChan chan x'
                    writeChan chan (f64Sqrt RoundNearEven x')
                    yield
                    if f64Lt u x' then go 9 else go x'
      in go 100
  return chan

one = ui64ToF64 RoundNearEven 1
zero = ui64ToF64 RoundNearEven 0

-- | Attempts to continually effect globals: exceptions and rounding mode
fudgeGlobals :: IO ()
fudgeGlobals = do
  forever $ do
    let try x = when (f64Eq one x) exitFailure
    try $ f64Div RoundNearEven one zero
    try $ f64Div RoundMinMag one zero
    try $ f64Div RoundMin one zero
    try $ f64Div RoundMax one zero
    try $ f64Div RoundNearMaxMag one zero
    try $ f64Div RoundOdd one zero

    try $ f64Add RoundNearEven one one
    try $ f64Add RoundNearEven one one
    try $ f64Add RoundMinMag one one
    try $ f64Add RoundMin one one
    try $ f64Add RoundMax one one
    try $ f64Add RoundNearMaxMag one one
    try $ f64Add RoundOdd one one

    yield


main :: IO ()
main = do
  putStrLn "Fudging globals..."
  forkIO fudgeGlobals
  forkIO fudgeGlobals
  forkIO fudgeGlobals
  putStrLn "Looking for non-determinism..."
  ca <- forkA
  cb <- forkA
  cc <- forkA
  let
    go :: Int -> IO ()
    go i = if i == 1000000
      then putStrLn "... still fine" >> go 0
      else do
        a <- readChan ca
        b <- readChan cb
        c <- readChan cc
        if a /= b || a /= c
          then do
            print (a,b,c)
            putStrLn "Non-determinism!!!!"
          else go (i+1)
  go 0
