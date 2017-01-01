### PayPal Client for Yesod ###

Integration of paypal-rest-client with Yesod.

#### Warning Regarding Stability

The paypal-rest-client library is an experimental package which asks the user to
specify an exact version. It is not expected that experimental release changes
in that package will affect this package, so we currently only require a version
lower than 1.0.0. Please specify an exact version of paypal-rest-client in your
application.

#### Example Usage

The following code demonstrates how to use this package with an example site.

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Config as Config
import Network.Payments.PayPal
import Network.Payments.PayPal.Environment
import Network.Payments.PayPal.Payments
import Yesod
import Yesod.Payments.PayPal

-- Create site.
data Example = Example YesodPayPalState

-- Create routes.
mkYesod "Example" [parseRoutes|
/ HomeR GET
|]

-- Site is instance of Yesod.
instance Yesod Example where
  approot = ApprootStatic "http://localhost:3000"

-- Site will use PayPal.
instance YesodPayPal Example where
  yesodPayPalCredentials _ = credentials
  yesodPayPalState (Example ppState) = ppState

-- Home page displays information about all transactions.
getHomeR :: Handler Html
getHomeR = do
  listResult <- yesodExecPayPal $ listPayments Nothing
  case listResult of
    Left err -> defaultLayout [whamlet|Error: #{show err}|]
    Right result -> defaultLayout [whamlet|Amount of payments: #{show result}|]

-- Start PayPal and start server.
main :: IO ()
main = do
  -- Get new PayPal state, including token.
  ppStateOrErr <- mkYesodPayPalState credentials
  case ppStateOrErr of
    Just ppState -> warp 3000 $ Example ppState
    Nothing -> error "Could not start server due to missing PayPal token."

-- PayPal credentials.
credentials :: PayPalCredentials
credentials = (sandboxUrl, Config.clientId, Config.secret)
```
