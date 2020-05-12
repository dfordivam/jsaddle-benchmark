module Main where

import Language.Javascript.JSaddle.Benchmark
import Language.Javascript.JSaddle.Warp

main :: IO ()
main = do
  putStrLn "Starting JSaddle Benchmark"
  run 3709 runBMs
