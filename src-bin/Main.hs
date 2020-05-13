{-# LANGUAGE BangPatterns #-}
module Main where

import Language.Javascript.JSaddle.Benchmark
import Language.Javascript.JSaddle.Warp
import System.Environment

main :: IO ()
main = do
  putStrLn $ "Optional arguments: port count benchmark-prefix"
  putStrLn $ "Example: jsaddle-benchmark 8000 500 \"makeObject only\""
  args <- getArgs
  let (!port, !count, !bm) = case args of
        [] -> (3709, Nothing, Nothing)
        (p:[]) -> (read p, Nothing, Nothing)
        (p:count:[]) -> (read p, Just (read count), Nothing)
        (p:count:bm:_) -> (read p, Just (read count), Just bm)
  putStrLn $ "Starting JSaddle Benchmark on port: " <> show port
  run port $ runBMs count bm
