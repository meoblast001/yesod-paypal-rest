-- |
-- Module: Main
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Config as Config
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Data.Time.Format
import Network.Payments.PayPal
import Network.Payments.PayPal.Environment
import Network.Payments.PayPal.Payments
import Network.Payments.PayPal.Types.Currency
import Network.Payments.PayPal.Types.Payer
import Network.Payments.PayPal.Types.Transaction
import Text.Cassius
import Yesod
import Yesod.Payments.PayPal

data Example = Example YesodPayPalState

mkYesod "Example" [parseRoutes|
/ HomeR GET
/payments PaymentsR GET
/payments/new NewPaymentR GET POST
/payments/success PaymentSuccessR GET
|]

instance Yesod Example where
  approot = ApprootStatic "http://localhost:3000"

instance YesodPayPal Example where
  yesodPayPalCredentials _ = credentials
  yesodPayPalState (Example ppState) = ppState

instance RenderMessage Example FormMessage where
  renderMessage _ _ = defaultFormMessage

type UrlRenderFunction = Route Example -> T.Text

getHomeR :: Handler Html
getHomeR = redirect PaymentsR

getPaymentsR :: Handler Html
getPaymentsR = do
  listResult <- yesodExecPayPal $ listPayments Nothing
  case listResult of
    Left err -> defaultLayout [whamlet|Error: #{show err}|]
    Right result ->
      let payments = listResPayments result
      in defaultLayout $(whamletFile "list_payments.hamlet")

getNewPaymentR :: Handler Html
getNewPaymentR = do
  ((ppRes, ppForm), ppEncType) <- runFormPost $ renderDivs $
                                  createPaymentFormPp Nothing
  case ppRes of
    FormSuccess result -> do
      urlRenderFunc <- getUrlRender
      let mayRequest = createPaymentFormPpToRequest result urlRenderFunc
      createResult <- case mayRequest of
          Just request ->
            yesodExecPayPal $ createPayment request
          Nothing -> return $ Left $ OtherError "Form incorrect."
      case createResult of
        Left err -> defaultLayout [whamlet|Error: #{show err}|]
        Right result ->
          case approvalUrlFromCreate result of
            Just url ->
              redirect url
            Nothing ->
              defaultLayout [whamlet|Created Payment: #{createResPayId result}|]
    _ -> defaultLayout $(whamletFile "new_payment.hamlet")

postNewPaymentR :: Handler Html
postNewPaymentR = getNewPaymentR

data CreatePayPalPaymentForm =
  CreatePayPalPaymentForm
  { cPpPayFormItemName :: T.Text
  , cPpPayFormItemDescription :: T.Text
  , cPpPayFormItemSubtotal :: T.Text
  , cPpPayFormItemShipping :: T.Text
  , cPpPayFormItemTax :: T.Text
  } deriving (Show)

createPaymentFormPp :: Maybe CreatePayPalPaymentForm ->
                       AForm Handler CreatePayPalPaymentForm
createPaymentFormPp mayForm =
  CreatePayPalPaymentForm <$>
  areq textField "Item Name" (cPpPayFormItemName <$> mayForm) <*>
  areq textField "Item Description" (cPpPayFormItemDescription <$> mayForm) <*>
  areq textField "Item Subtotal" (cPpPayFormItemSubtotal <$> mayForm) <*>
  areq textField "Item Shipping" (cPpPayFormItemShipping <$> mayForm) <*>
  areq textField "Item Tax" (cPpPayFormItemTax <$> mayForm)

createPaymentFormPpToRequest :: CreatePayPalPaymentForm -> UrlRenderFunction ->
                                Maybe CreateRequest
createPaymentFormPpToRequest form urlRenderFunc = do
  let payer = Payer PayMethodPayPal [] Nothing Nothing
  subtotal <- realToFrac <$> fst <$>
              (eitherToMaybe $ TR.double $ cPpPayFormItemSubtotal form)
  shipping <- realToFrac <$> fst <$>
              (eitherToMaybe $ TR.double $ cPpPayFormItemShipping form)
  tax <- realToFrac <$> fst <$>
         (eitherToMaybe $ TR.double $ cPpPayFormItemTax form)
  let total = shipping + subtotal + tax
      details = Details shipping subtotal tax
      amount = Amount EUR total details
      item = Item 1 (T.unpack $ cPpPayFormItemName form) subtotal EUR "Example"
                  (Just $ T.unpack $ cPpPayFormItemDescription form)
      itemList = ItemList [item] Nothing
      transaction = Transaction amount
                    (Just $ T.unpack $ cPpPayFormItemDescription form) itemList
      redirectUrls = RedirectUrls (T.unpack $ urlRenderFunc PaymentSuccessR)
                                  (T.unpack $ urlRenderFunc HomeR)
  return $ CreateRequest SaleIntent payer [transaction] $ Just redirectUrls

getPaymentSuccessR :: Handler Html
getPaymentSuccessR = do
  getParams <- reqGetParams <$> getRequest
  let getParamsBS = M.fromList $
                    map (\(a, b) -> (TE.encodeUtf8 a, TE.encodeUtf8 b))
                        getParams
      returnParams = returnLinkParams getParamsBS
  case returnParams of
    Just (ReturnLinkParams payId _ payerId) -> do
      executeResult <- yesodExecPayPal $
                       executePayment payId (ExecuteRequest payerId [])
      case executeResult of
        Left err -> defaultLayout [whamlet|Error: #{show err}|]
        Right result ->
          defaultLayout [whamlet|#{show $ executeResHateoasLinks result}|]
    Nothing ->
      defaultLayout [whamlet|Could not parse return link params.|]

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

main :: IO ()
main = do
  ppStateOrErr <- mkYesodPayPalState credentials
  case ppStateOrErr of
    Just ppState -> warp 3000 $ Example ppState
    Nothing -> error "Could not start server due to missing PayPal token."

credentials :: PayPalCredentials
credentials = (sandboxUrl, Config.clientId, Config.secret)
