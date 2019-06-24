{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Config
  ( App (..)
  , runApp
  ) where

import RIO
import System.Posix.Env (getEnv)

data App = App
  { appLogFunc :: !LogFunc }
instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })


runApp :: MonadIO m => RIO App a -> m a
runApp thunk = liftIO $ do
  verbose <- isJust <$> getEnv "RIO_VERBOSE"
  logOptions <- setLogUseColor False
                <$> setLogUseTime verbose
                <$> setLogUseLoc verbose
                <$> logOptionsHandle stdout True
  withLogFunc logOptions $ \logFunc ->
    let app = App { appLogFunc = logFunc }
    in runRIO app thunk
