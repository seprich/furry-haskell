{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RIO hiding (traceId)
import qualified RIO.List as L
import qualified RIO.ByteString as B
import qualified RIO.Text as T

import System.Posix.Env (getEnv, setEnv, unsetEnv)
import Data.List.Split (splitOn)

import Config (runApp)
import qualified AwsLambdaClient as CLI
import Lib.Models (InvocationContext(..), ErrorResponse(..))
import Lib.Lambda (lambdaFunction)


data InitException = InitException String deriving (Typeable, Show)
instance Exception InitException

data InitData = InitData
  { awsLambdaHost :: B.ByteString
  , awsLambdaPort :: Int
  , awsLambdaTaskRoot :: String
  , awsLambdaHandler :: String
  } deriving (Show, Eq)


executeInvocation :: (HasLogFunc app) => InitData -> RIO app ()
executeInvocation initData =
  let host = awsLambdaHost initData
      port = awsLambdaPort initData
  in do
    (context, content) <- CLI.nextInvocation host port
    liftIO $ setEnv "_X_AMZN_TRACE_ID" (traceId context) True
    catchAny (executeAndRespond host port context content) (handleError host port context)
    liftIO $ unsetEnv "_X_AMZN_TRACE_ID"
    executeInvocation initData
  where
    executeAndRespond host port context content = do
      response <- lambdaFunction context content
      CLI.invocationResponse host port (awsRequestId context) response
    handleError host port context error = do
      CLI.invocationError host port (awsRequestId context) (ErrorResponse {errorType = "?", errorContent = ""})


resolveHostAndPort :: (MonadIO m) => m (B.ByteString, Int)
resolveHostAndPort = liftIO $ do
  maybeParts <- (fmap . fmap) (splitOn ":") (getEnv "AWS_LAMBDA_RUNTIME_API")
  maybeHost <- return (maybeParts >>= L.initMaybe >>= (return . L.intercalate ":"))
  maybePort <- return (maybeParts >>= L.lastMaybe >>= readMaybe)
  case (maybeHost, maybePort) of
    (Just host, Just port) -> return ((T.encodeUtf8 . T.pack) host, port)
    _                      -> throwM (InitException "Malformatted AWS_LAMBDA_RUNTIME_API env var")


initialize :: (HasLogFunc app) => RIO app InitData
initialize = do
  (host, port) <- resolveHostAndPort
  catchAny (initRest host port) (handleError host port)
  where
    initRest host port = do
      -- TODO INITIALIZE MORE
      return $ InitData {
        awsLambdaHost = host,
        awsLambdaPort = port,
        awsLambdaTaskRoot = "",
        awsLambdaHandler = ""
      }
    handleError host port error = do
      CLI.initError host port (ErrorResponse {errorType = "?", errorContent = ""})
      throwM error 


main :: IO ()
main = runApp $ catchAny
  (initialize >>= executeInvocation)
  (\error -> logError $ fromString $ show error)
