{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module AwsLambdaClient
  ( nextInvocation
  , invocationResponse
  , invocationError
  , initError
  ) where

import RIO
import qualified RIO.List as L
import qualified RIO.ByteString as B
import qualified RIO.Text as T

import Data.Aeson (Value)
import Data.Aeson.Types (FromJSON)
import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Types.Header (HeaderName)

import Lib.Models (InvocationContext(..), ErrorResponse(..))

data RecoverableException = RecoverableException String deriving (Typeable, Show)
instance Exception RecoverableException

data NonRecoverableException = NonRecoverableException deriving (Typeable, Show)
instance Exception NonRecoverableException


managerLong :: (MonadIO m) => m Manager
managerLong = liftIO $ newManager $ defaultManagerSettings { managerResponseTimeout = responseTimeoutNone }

managerShort :: (MonadIO m) => m Manager
managerShort = liftIO $ newManager $ defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro (5 * 1000000) }

decodeUtf8 bytestring = T.unpack $ T.decodeUtf8With T.lenientDecode bytestring
getStringHeader name response = case L.headMaybe (fmap decodeUtf8 $ getResponseHeader name response) of
  Just x -> x
  Nothing -> ""
getIntHeader :: HeaderName -> Response a -> Int
getIntHeader name response = case L.headMaybe (fmap (readMaybe . decodeUtf8) $ getResponseHeader name response) of
  Just (Just x) -> x
  _ -> 0


makeRequest :: (HasLogFunc app) => Manager -> B.ByteString -> B.ByteString -> Int -> B.ByteString -> Maybe Value -> (Response Value -> b) -> RIO app b
makeRequest manager method host port path maybeContent extractor = do
  let request
        = setRequestMethod method
        $ setRequestHost host
        $ setRequestPort port
        $ setRequestPath path
        $ (case maybeContent of Just content -> setRequestBodyJSON content ; Nothing -> id)
        $ setRequestManager manager
        $ defaultRequest
  response <- httpJSON request
  case (getResponseStatusCode response) of
    status | status == 202 || status == 200 -> return (extractor response)
    status | status >= 400 && status <= 499 -> do
      let message = "BadRequest " ++ show status   -- TODO parse body too
      logError $ fromString message
      throwIO $ RecoverableException message
    status -> do
      logError $ fromString ("Unexpected http response " ++ show status)
      throwIO NonRecoverableException


nextInvocation :: (HasLogFunc app) => B.ByteString -> Int -> RIO app (InvocationContext, Value)
nextInvocation host port = do
  let path = "/2018-06-01/runtime/invocation/next"
  manager <- managerLong
  makeRequest manager "GET" host port path Nothing extractor `catches`
    [Handler handleNonRecoverable,
     Handler handleRecoverable]
  where
    extractor response =
      (InvocationContext {
        awsRequestId = getStringHeader "Lambda-Runtime-Aws-Request-Id" response,
        deadlineMs = getIntHeader "Lambda-Runtime-Deadline-Ms" response,
        invokedFunctionArn = getStringHeader "Lambda-Runtime-Invoked-Function-Arn" response,
        traceId = getStringHeader "Lambda-Runtime-Trace-Id" response,
        clientContext = getStringHeader "Lambda-Runtime-Client-Context" response,
        cognitoIdentity = getStringHeader "Lambda-Runtime-Cognito-Identity" response
        }, getResponseBody response)
    handleNonRecoverable :: NonRecoverableException -> RIO app (InvocationContext, Value)
    handleNonRecoverable e = throwIO e
    handleRecoverable :: (HasLogFunc app) => SomeException -> RIO app (InvocationContext, Value)
    handleRecoverable e = nextInvocation host port


responseConsumer :: Response Value -> ()
responseConsumer _ = ()

invocationResponse :: (HasLogFunc app) => B.ByteString -> Int -> String -> Value -> RIO app ()
invocationResponse host port awsRequestId content = do
  let path = B.concat ["/2018-06-01/runtime/invocation/", (T.encodeUtf8 . T.pack) awsRequestId, "/response"]
  manager <- managerShort
  makeRequest manager "POST" host port path (Just content) responseConsumer


-- TODO: addRequestHeader "Lambda-Runtime-Function-Error-Type" ((T.encodeUtf8 . T.pack) (errorType content))
sendError :: (HasLogFunc app) => B.ByteString -> Int -> B.ByteString -> ErrorResponse -> RIO app ()
sendError host port path content = do
  manager <- managerShort
  makeRequest manager "POST" host port path (Just (errorContent content)) responseConsumer


invocationError :: (HasLogFunc app) => B.ByteString -> Int -> String -> ErrorResponse -> RIO app ()
invocationError host port awsRequestId content = sendError host port (B.concat ["/2018-06-01/runtime/invocation/", ((T.encodeUtf8 . T.pack) awsRequestId), "/error"]) content

initError :: (HasLogFunc app) => B.ByteString -> Int -> ErrorResponse -> RIO app ()
initError host port content = sendError host port "/2018-06-01/runtime/init/error" content
