{-# LANGUAGE OverloadedStrings #-}
module AwsLambdaClient
  ( nextInvocation
  , invocationResponse
  , invocationError
  , initError
  ) where

import Control.Exception.Safe (Exception, SomeException, catchAny, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU
import Data.Typeable (Typeable)
import Data.Aeson (Value)
import Network.HTTP.Simple
import Network.HTTP.Client

import Lib.Models (InvocationContext(..), ErrorResponse(..))

data RecoverableException = RecoverableException String deriving (Typeable, Show)
instance Exception RecoverableException

data NonRecoverableException = NonRecoverableException deriving (Typeable, Show)
instance Exception NonRecoverableException


logError :: (Show a) => a -> IO ()
logError msg = putStrLn $ show msg

managerLong :: IO Manager
managerLong = newManager $ defaultManagerSettings { managerResponseTimeout = responseTimeoutNone }

managerShort :: IO Manager
managerShort = newManager $ defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro (5 * 1000000) }

getStringHeader name response = head $ (fmap BSU.toString $ getResponseHeader name response) ++ [""]
getIntHeader name response = head $ (fmap (read . BSU.toString) $ getResponseHeader name response) ++ [0]

nextInvocation :: (MonadIO m) => B.ByteString -> Int -> m (InvocationContext, Value)
nextInvocation host port = liftIO (catchAny fetchNextInvocation handleError)
  where
    fetchNextInvocation = do
      manager <- managerLong
      let request
            = setRequestMethod "GET"
            $ setRequestHost host
            $ setRequestPort port
            $ setRequestPath "/2018-06-01/runtime/invocation/next"
            $ setRequestManager manager
            $ defaultRequest
      response <- httpJSON request
      case (getResponseStatusCode response) of
        200 -> return $ (InvocationContext {
                          awsRequestId = getStringHeader "Lambda-Runtime-Aws-Request-Id" response,
                          deadlineMs = getIntHeader "Lambda-Runtime-Deadline-Ms" response,
                          invokedFunctionArn = getStringHeader "Lambda-Runtime-Invoked-Function-Arn" response,
                          traceId = getStringHeader "Lambda-Runtime-Trace-Id" response,
                          clientContext = getStringHeader "Lambda-Runtime-Client-Context" response,
                          cognitoIdentity = getStringHeader "Lambda-Runtime-Cognito-Identity" response
                          }, getResponseBody response)
        500 -> throwM NonRecoverableException
        n -> throwM $ RecoverableException ("Error Status" ++ show n)  -- TODO properly json parse the body
    handleError e = do  --TODO NonRecoverableException should escape the retry loop
        logError e
        nextInvocation host port


invocationResponse :: (MonadIO m) => B.ByteString -> Int -> String -> Value -> m ()
invocationResponse host port awsRequestId content = liftIO $ do
  manager <- managerShort
  let request
        = setRequestMethod "POST"
        $ setRequestHost host
        $ setRequestPort port
        $ setRequestPath (B.concat ["/2018-06-01/runtime/invocation/", (BSU.fromString awsRequestId), "/response"])
        $ setRequestBodyJSON content
        $ setRequestManager manager
        $ defaultRequest
  response <- (httpLBS request)
  case (getResponseStatusCode response) of
    status | status == 202 || status == 200 -> return ()
    status | status >= 400 && status <= 499 -> throwM $ RecoverableException "Bad Request"  -- TODO properly parse body
    _ -> throwM NonRecoverableException


sendError :: (MonadIO m) => B.ByteString -> Int -> B.ByteString -> ErrorResponse -> m ()
sendError host port path content = liftIO $ do
  manager <- managerShort
  let request
        = setRequestMethod "POST"
        $ setRequestHost host
        $ setRequestPort port
        $ setRequestPath path
        $ addRequestHeader "Lambda-Runtime-Function-Error-Type" (BSU.fromString (errorType content))
        $ setRequestBodyJSON (errorContent content)
        $ setRequestManager manager
        $ defaultRequest
  response <- (httpLBS request)
  case (getResponseStatusCode response) of
    status | status == 202 || status == 200 -> return ()
    status | status >= 400 && status <= 499 -> throwM $ RecoverableException "Bad Request"  -- TODO properly parse body
    _ -> throwM NonRecoverableException

invocationError :: (MonadIO m) => B.ByteString -> Int -> String -> ErrorResponse -> m ()
invocationError host port awsRequestId content = sendError host port (B.concat ["/2018-06-01/runtime/invocation/", (BSU.fromString awsRequestId), "/error"]) content

initError :: (MonadIO m) => B.ByteString -> Int -> ErrorResponse -> m ()
initError host port content = sendError host port "/2018-06-01/runtime/init/error" content
