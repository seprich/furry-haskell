{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List.Split (splitOn)
import Data.List (intercalate)
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception.Safe (Exception, catchAny, throwM)
import System.Environment (getEnv)
import System.Posix.Env (setEnv, unsetEnv)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU

import qualified AwsLambdaClient as CLI
import Lib.Models (InvocationContext(..), ErrorResponse(..))
import Lib.Lambda (lambdaFunction)

data InitException = InitException String deriving (Typeable, Show)
instance Exception InitException

logError :: (Show a) => a -> IO ()
logError msg = putStrLn $ show msg


data InitData = InitData
  { awsLambdaHost :: B.ByteString
  , awsLambdaPort :: Int
  , awsLambdaTaskRoot :: String
  , awsLambdaHandler :: String
  } deriving (Show, Eq)


executeInvocation :: (MonadIO m) => InitData -> m ()
executeInvocation initData =
  let host = awsLambdaHost initData
      port = awsLambdaPort initData
  in do
    (context, content) <- CLI.nextInvocation host port
    liftIO $ setEnv "_X_AMZN_TRACE_ID" (traceId context) True
    liftIO $ catchAny (executeAndRespond host port context content) (handleError host port context)
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
  hostAndPort <- getEnv "AWS_LAMBDA_RUNTIME_API" >>= return . splitOn ":"
  if length hostAndPort < 2 then throwM (InitException "Malformatted AWS_LAMBDA_RUNTIME_API env var") else return ()
  return (BSU.fromString (intercalate ":" (init hostAndPort)), read (last hostAndPort))

initialize :: (MonadIO m) => m InitData
initialize = do
  (host, port) <- resolveHostAndPort
  liftIO $ catchAny (initRest host port) (handleError host port)
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


main :: (MonadIO m) => m ()
main = liftIO $ catchAny
  (do
    initData <- initialize
    executeInvocation initData)
  (\error -> logError error)
