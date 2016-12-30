-- |
-- Module: Yesod.Payments.PayPal
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Yesod.Payments.PayPal
( PayPalCredentials
, YesodPayPalState
, mkYesodPayPalState
, YesodPayPal(..)
, yesodExecPayPal
) where

import Data.IORef
import Data.Time.Clock
import Network.Payments.PayPal
import Network.Payments.PayPal.Auth
import Network.Payments.PayPal.Environment
import Yesod.Core

-- |All credentials needed to log into PayPal's API.
type PayPalCredentials = (EnvironmentUrl, ClientID, Secret)

-- |State needed over multiple PayPal requests.
data YesodPayPalState =
  YesodPayPalState { yPpStToken :: IORef (AccessToken, UTCTime) }

-- |Create a PayPal state for a site.
mkYesodPayPalState :: PayPalCredentials -> IO (Maybe YesodPayPalState)
mkYesodPayPalState (envUrl, clientId, secret) = do
  accessTokenOrErr <- fetchAccessTokenWithExpiration envUrl clientId secret
  case accessTokenOrErr of
    Left err -> return Nothing
    Right accessToken -> do
      ref <- newIORef accessToken
      return $ Just $ YesodPayPalState ref

-- |Typeclass for Yesod sites which use PayPal.
class Yesod site => YesodPayPal site where
  -- |The PayPal API credentials for a site.
  yesodPayPalCredentials :: site -> PayPalCredentials
  -- |State needed over multiple PayPal requests for a site.
  yesodPayPalState :: site -> YesodPayPalState

-- |Execute PayPal operations in the context of Yesod.
yesodExecPayPal :: (YesodPayPal site, FromJSON a) =>
                   PayPalOperations a -> HandlerT site IO (Either PayPalError a)
yesodExecPayPal operations = do
  site <- getYesod
  let (envUrl, clientId, secret) = yesodPayPalCredentials site
      accessTokenRef = yPpStToken $ yesodPayPalState site
  liftIO $ do
    accessToken <- readIORef accessTokenRef
    resultOrErr <- execPayPalOpers envUrl clientId secret accessToken operations
    case resultOrErr of
      Left err -> return $ Left err
      Right (result, accessToken') -> do
        writeIORef accessTokenRef accessToken'
        return $ Right result
