{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad.IO.Class
import Language.Javascript.JSaddle.Benchmark
import Language.Javascript.JSaddle.Wasm

main :: IO ()
main = do
  putStrLn $ "Starting JSaddle Benchmark"
  run 0 $ do
    results <- runBMs (Just 100) Nothing
    putResultsInDom results
    liftIO $ printResults results
