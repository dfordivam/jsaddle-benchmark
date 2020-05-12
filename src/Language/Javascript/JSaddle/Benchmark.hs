{-# LANGUAGE BangPatterns #-}

module Language.Javascript.JSaddle.Benchmark where

import Control.Monad.Reader
import Data.Time
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
  runReaderT (measureElapsedTime 1000 (sequence valToTests)) testData

makeTestData :: JSM TestData
makeTestData = do
  !b <- toJSVal True
  !n <- toJSVal (2.56 :: Double)
  !s <- toJSVal ("example string" :: String)
  !o <- do
    o <- create
    (o <# "boolean_val") b
    (o <# "number_val") n
    (o <# "string_val") s
    toJSVal o
  pure $ TestData b n s o

measureElapsedTime :: (MonadJSM m) => Int -> m a -> m ()
measureElapsedTime c f = do
  startTime <- liftIO $ getCurrentTime
  replicateM_ c f
  endTime <- liftIO $ getCurrentTime
  liftIO $ print $ diffUTCTime endTime startTime

valToTests :: [TestM ()]
valToTests =
  [ doValToBool
  , doValToNumber
  , doValToStr
  , doValToText
  , doValToObject
  , doValToJSON
  ]

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
