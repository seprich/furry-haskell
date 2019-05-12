{-# LANGUAGE OverloadedStrings #-}
module Lib.Lambda
  ( lambdaFunction
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value)

import Lib.Models (InvocationContext(..), ErrorResponse(..))


lambdaFunction :: (MonadIO m) => InvocationContext -> Value -> m Value
lambdaFunction context payload = liftIO $ return payload
