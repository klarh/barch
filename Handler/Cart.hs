{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Cart where

import Import
import Barch.Cart
import Barch.Widgets (shortReferenceView)
import Data.Maybe (catMaybes)

postAddCartR::ReferenceId->Handler ()
postAddCartR refid = do
  addToCart refid
  redirect ViewCartR

postRemoveCartR::ReferenceId->Handler ()
postRemoveCartR refid = do
  removeFromCart refid
  redirect ViewCartR

getViewCartR::Handler Html
getViewCartR = do
  cartIds <- listCart
  maybeCartItems <- mapM runDB $ get <$> cartIds
  let cartItems = catMaybes . zipWith (\key val -> Entity key <$> val) cartIds $ maybeCartItems

  defaultLayout $ do
    aDomId <- newIdent
    setTitle "Barch: View Cart"
    $(widgetFile "cart")
