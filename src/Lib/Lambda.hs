{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib.Lambda
  ( lambdaFunction
  ) where

import RIO
import qualified RIO.Text as T
import qualified RIO.ByteString.Lazy as BL
import Data.Aeson (Value, encode)

import Lib.Models (InvocationContext(..), ErrorResponse(..))


lambdaFunction :: (HasLogFunc app) => InvocationContext -> Value -> RIO app Value
lambdaFunction context payload = do
  logInfo $ fromString $ T.unpack $ T.decodeUtf8With(T.lenientDecode) $ BL.toStrict $ encode payload
  return payload
