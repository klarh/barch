{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Cart where

import Import
import Barch.Adaptors (reference2Entry)
import Barch.Cart
import Barch.Widgets (shortReferenceView)
import Data.Monoid (mconcat)
import qualified Text.BibTeX.Format as BibF

postAddCartR::ReferenceId->Handler ()
postAddCartR refid = do
  addToCart refid
  redirect ViewCartR

postRemoveCartR::ReferenceId->Handler ()
postRemoveCartR refid = do
  removeFromCart refid
  redirect ViewCartR

postClearCartR::Handler ()
postClearCartR = do
  clearCart
  redirect HomeR

getViewCartR::Handler Html
getViewCartR = do
  cartItems <- listCartItems

  defaultLayout $ do
    aDomId <- newIdent
    setTitle "Barch: View Cart"
    $(widgetFile "cart")

getExportCartR::Handler Html
getExportCartR = do
  entries <- map (BibF.entry . reference2Entry . entityVal) <$> listCartItems

  defaultLayout $ do
    aDomId <- newIdent
    setTitle "Barch: Export Cart"
    $(widgetFile "exportCart")

getExportCartBibR::Handler TypedContent
getExportCartBibR = do
  items <- listCartItems

  let bibEntries = BibF.entry . reference2Entry . entityVal <$> items
      bibText = toContent . mconcat $ bibEntries

  return $ TypedContent typePlain bibText
