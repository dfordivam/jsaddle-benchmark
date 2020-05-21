{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Traversable
import qualified Data.Text as T
import Reflex.Dom
import Language.Javascript.JSaddle.Benchmark
import Language.Javascript.JSaddle
import Safe

main :: IO ()
main = mainWidget $ do
  text "JSaddle BM"
  countVal <- (((T.unpack <$>) . value) <$>) $ el "p" $ do
    text "Count (default 1000)"
    textInput def
  bmName <- (((T.unpack <$>) . value) <$>) $ el "p" $ do
    text "BM filter prefix (optional)"
    textInput def
  ev <- el "p" $ button "Run"
  display =<< count ev
  resultEv <- performEvent $ ffor (tag (current ((,) <$> countVal <*> bmName)) ev) $ \(c, n) -> do
    liftJSM $ runBMs (readMay c) (if null n then Nothing else Just n)
  el "div" $ widgetHold_ (text "Results will be shown below")
    (makeResultTables <$> resultEv)

makeResultTables :: (DomBuilder t m) => BMResults -> m ()
makeResultTables results = do
  el "p" $ el "table" $ do
    el "th" $ do
      el "td" $ text "Test name"
      el "td" $ text "Time (in sec)"
    for_ (results :: BMResults) $ \(d, t) -> el "tr" $ do
      el "td" $ text d
      el "td" $ text $ T.pack $ (init $ show t)
  -- Print just time for easier copy-paste
  el "p" $ el "table" $ do
    el "th" $ do
      el "td" $ text "Time (in sec)"
    for_ (results :: BMResults) $ \(_, t) -> el "tr" $ do
      el "td" $ text $ T.pack $ (init $ show t)
