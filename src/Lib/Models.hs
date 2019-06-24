{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Lib.Models
    ( InvocationContext (..)
    , ErrorResponse (..)
    ) where
import RIO
import Data.Aeson (Value)

data InvocationContext = InvocationContext
    { awsRequestId :: String
    , deadlineMs :: Int
    , invokedFunctionArn :: String
    , traceId :: String
    , clientContext :: String
    , cognitoIdentity :: String
    } deriving (Show, Eq)


data ErrorResponse = ErrorResponse
    { errorType :: String
    , errorContent :: Value
    } deriving (Show, Eq)
