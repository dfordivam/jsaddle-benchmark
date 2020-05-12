{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Javascript.JSaddle.Benchmark where

import Control.Lens
import Control.Monad.Reader
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
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
type BMResults = [(Text, NominalDiffTime)]

runBMs :: JSM ()
runBMs = do
  !testData <- makeTestData
  results <- runReaderT (runTestAndGatherResult 1000) testData
  putResultsInDom results
  liftIO $ for_ results $ \(desc, t) -> do
    putStrLn $ (T.unpack desc) <> "\t\t" <> show t
  -- results <- runReaderT (measureElapsedTime 1000 doSetPropNumber) testData
  -- b <- valToBool =<< (_testData_objVar testData) ^. js ("boolean_val" :: JSString)
  -- liftIO $ print b
  -- liftIO $ print results

putResultsInDom :: BMResults -> JSM ()
putResultsInDom results = do
  let
    innerHTML :: Text
    innerHTML = table $ ([ rowHeader ] <>) $
      (flip map) results $ \(desc, t) -> row [td desc, td (T.pack $ show t)]
    table :: [Text] -> Text
    table c = "<table>" <> mconcat c <> "</table>"
    row :: [Text] -> Text
    row c = "<tr>" <> mconcat c <> "</tr>"
    td :: Text -> Text
    td c = "<td>" <> c <> "</td>"
    rowHeader :: Text
    rowHeader = "<th>Description</th><th>time (in sec)</th>"
  doc <- jsg ("document" :: JSString)
  doc ^. js ("body" :: JSString) ^. jss ("innerHTML" :: JSString) innerHTML

runTestAndGatherResult :: Int -> TestM BMResults
runTestAndGatherResult c = do
  for allTests $ \(test, description) -> do
    (description,) <$> measureElapsedTime c test
  where
    allTests = valToTests
      <> toJSValTests
      <> makeObjectTests
      <> makeArrayTests
      <> getSetProp
      <> apiCallTests

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

makeObjectTests =
  [ (doMakeObject, "makeObject")
  , (doMakeObjectAndGetProp, "makeObject + getProp")
  , (doMakeObjectAndToJSON, "makeObject + valToJSON")
  ]

makeArrayTests =
  [ (doMakeArray, "array")
  , (doMakeArrayAndPropertyNames, "array + propertyNames")
  , (doMakeArrayAndProperties, "array + properties")
  ]

getSetProp =
  [ (doGetPropBool, "getProp bool")
  , (doGetPropNumber, "getProp number")
  , (doGetPropText, "getProp text")
  , (doSetPropBool, "setProp bool")
  , (doSetPropNumber, "setProp number")
  , (doSetPropText, "setProp text")
  , (doGetSetPropBool, "getProp + setProp bool")
  , (doGetSetPropNumber, "getProp + setProp number")
  , (doGetSetPropText, "getProp + setProp text")
  ]

apiCallTests =
  [ (doApiCallLength, "call string length")
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

doMakeObject :: TestM ()
doMakeObject = do
  b <- asks _testData_boolVar
  n <- asks _testData_numberVar
  s <- asks _testData_strVar
  !res <- lift $ do
    o <- create
    (o <# ("boolean_val" :: String)) b
    (o <# ("number_val" :: String)) n
    (o <# ("string_val" :: String)) s
    toJSVal o
  pure ()

doMakeObjectAndGetProp :: TestM ()
doMakeObjectAndGetProp = do
  b <- asks _testData_boolVar
  n <- asks _testData_numberVar
  s <- asks _testData_strVar
  !res <- lift $ do
    o <- create
    (o <# ("boolean_val" :: String)) b
    (o <# ("number_val" :: String)) n
    (o <# ("string_val" :: String)) s
    getProp "boolean_val" o
  pure ()

doMakeObjectAndToJSON :: TestM ()
doMakeObjectAndToJSON = do
  b <- asks _testData_boolVar
  n <- asks _testData_numberVar
  s <- asks _testData_strVar
  !res <- lift $ do
    o <- create
    (o <# ("boolean_val" :: String)) b
    (o <# ("number_val" :: String)) n
    (o <# ("string_val" :: String)) s
    valToJSON =<< toJSVal o
  pure ()

doMakeArray :: TestM ()
doMakeArray = do
  !res <- lift $ do
    array ("Hello" :: Text, JSNull, (), True, 1.0::Double)
  pure ()

doMakeArrayAndPropertyNames :: TestM ()
doMakeArrayAndPropertyNames = do
  !res <- lift $ do
    propertyNames =<< array ("Hello" :: Text, JSNull, (), True, 1.0::Double)
  pure ()

doMakeArrayAndProperties :: TestM ()
doMakeArrayAndProperties = do
  !res <- lift $ do
    properties =<< array ("Hello" :: Text, JSNull, (), True, 1.0::Double)
  pure ()

doApiCallLength :: TestM ()
doApiCallLength = do
  s <- asks _testData_strVar
  !res <- lift $ valToNumber =<< s ^. js ("length" :: JSString)
  pure ()

doGetPropBool :: TestM ()
doGetPropBool = do
  o <- asks _testData_objVar
  !res <- lift $ valToBool =<< o ^. js ("boolean_val" :: JSString)
  pure ()

doGetPropNumber :: TestM ()
doGetPropNumber = do
  o <- asks _testData_objVar
  !res <- lift $ valToNumber =<< o ^. js ("number_val" :: JSString)
  pure ()

doGetPropText :: TestM ()
doGetPropText = do
  o <- asks _testData_objVar
  !res <- lift $ valToText =<< o ^. js ("string_val" :: JSString)
  pure ()

doSetPropBool :: TestM ()
doSetPropBool = do
  o <- asks _testData_objVar
  lift $ (o <# ("boolean_val" :: JSString)) False
  pure ()

doSetPropNumber :: TestM ()
doSetPropNumber = do
  o <- asks _testData_objVar
  lift $ (o <# ("number_val" :: JSString)) (5.5 :: Double)
  pure ()

doSetPropText :: TestM ()
doSetPropText = do
  o <- asks _testData_objVar
  lift $ (o <# ("string_val" :: JSString)) ("new string" :: JSString)
  pure ()

doGetSetPropBool :: TestM ()
doGetSetPropBool = do
  o <- asks _testData_objVar
  lift $ do
    b <- valToBool =<< o ^. js ("boolean_val" :: JSString)
    (o <# ("boolean_val" :: JSString)) (not b)
  pure ()

doGetSetPropNumber :: TestM ()
doGetSetPropNumber = do
  o <- asks _testData_objVar
  lift $ do
    n <- valToNumber =<< o ^. js ("number_val" :: JSString)
    (o <# ("number_val" :: JSString)) (n + 0.1 :: Double)
  pure ()

doGetSetPropText :: TestM ()
doGetSetPropText = do
  o <- asks _testData_objVar
  lift $ do
    v <- valToText =<< o ^. js ("string_val" :: JSString)
    (o <# ("string_val" :: JSString)) ("a" <> T.drop 1 v)
  pure ()
