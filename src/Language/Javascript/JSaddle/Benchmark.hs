{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Javascript.JSaddle.Benchmark where

import Control.Monad.Reader
import Data.Foldable
import Data.Text (Text)
import Data.Time
import Data.Traversable
import Language.Javascript.JSaddle

type TestM = ReaderT TestData JSM

data TestData = TestData
  { _testData_boolVar :: !JSVal
  , _testData_numberVar :: !JSVal
  , _testData_strVar :: !JSVal
  , _testData_objVar :: !JSVal
  }

runBMs :: JSM ()
runBMs = do
  !testData <- makeTestData
  results <- runReaderT (runTestAndGatherResult 1000) testData
  liftIO $ traverse_ print results

runTestAndGatherResult :: Int -> TestM [(Text, NominalDiffTime)]
runTestAndGatherResult c = do
  for allTests $ \(test, description) -> do
    (description,) <$> measureElapsedTime c test
  where
    allTests = valToTests <> toJSValTests

measureElapsedTime :: (MonadJSM m) => Int -> m a -> m (NominalDiffTime)
measureElapsedTime c f = do
  startTime <- liftIO $ getCurrentTime
  replicateM_ c f
  endTime <- liftIO $ getCurrentTime
  pure $ diffUTCTime endTime startTime

valToTests :: [(TestM (), Text)]
valToTests =
  [ (doValToBool, "valToBool")
  , (doValToNumber, "valToNumber")
  , (doValToStr, "valToStr")
  , (doValToText, "valToText")
  , (doValToObject, "valToObject")
  , (doValToJSON, "valToJSON")
  ]

toJSValTests :: [(TestM (), Text)]
toJSValTests =
  [ (doToJSValBool, "toJSVal Bool")
  , (doToJSValNumber, "toJSVal Double")
  , (doToJSValString, "toJSVal String")
  , (doToJSValText, "toJSVal Text")
  ]

makeTestData :: JSM TestData
makeTestData = do
  !b <- toJSVal True
  !n <- toJSVal (2.56 :: Double)
  !s <- toJSVal ("example string" :: String)
  !o <- do
    o <- create
    (o <# ("boolean_val" :: String)) b
    (o <# ("number_val" :: String)) n
    (o <# ("string_val" :: String)) s
    toJSVal o
  pure $ TestData b n s o

doValToBool :: TestM ()
doValToBool = do
  !res <- lift . valToBool =<< asks _testData_boolVar
  pure ()

doValToNumber :: TestM ()
doValToNumber = do
  !res <- lift . valToNumber =<< asks _testData_numberVar
  pure ()

doValToStr :: TestM ()
doValToStr = do
  !res <- lift . valToStr =<< asks _testData_strVar
  pure ()

doValToText :: TestM ()
doValToText = do
  !res <- lift . valToText =<< asks _testData_strVar
  pure ()

doValToObject :: TestM ()
doValToObject = do
  !res <- lift . valToObject =<< asks _testData_objVar
  pure ()

doValToJSON :: TestM ()
doValToJSON = do
  !res <- lift . valToJSON =<< asks _testData_objVar
  pure ()

doToJSValBool :: TestM ()
doToJSValBool = do
  !res <- lift $ valToBool =<< toJSVal True
  pure ()

doToJSValNumber :: TestM ()
doToJSValNumber = do
  !res <- lift $ valToNumber =<< toJSVal (3.56 :: Double)
  pure ()

doToJSValString :: TestM ()
doToJSValString = do
  !res <- lift $ valToStr =<< toJSVal ("a string" :: String)
  pure ()

doToJSValText :: TestM ()
doToJSValText = do
  !res <- lift $ valToStr =<< toJSVal ("a text string" :: Text)
  pure ()
