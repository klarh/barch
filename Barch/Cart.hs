{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Barch.Cart where

import Import
import Prelude
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set as S (Set, delete, empty, insert, toList)
import Data.Text (unpack, pack)

listCart::Handler [ReferenceId]
listCart = do
  oldCart <- lookupSession "citationCart"
  let cart = toList . fromMaybe empty $ read . unpack <$> oldCart
  return cart

listCartItems::Handler [Entity Reference]
listCartItems = do
  cartIds <- listCart
  maybeCartItems <- mapM runDB $ get <$> cartIds
  let cartItems = catMaybes . zipWith (\key val -> Entity key <$> val) cartIds $ maybeCartItems

  return cartItems

modifyCart::(Set ReferenceId->Set ReferenceId)->Handler ()
modifyCart f = do
  oldCart <- lookupSession "citationCart"
  let cart = f . fromMaybe empty $ read . unpack <$> oldCart
  setSession "citationCart" (pack . show $ cart)

addToCart::ReferenceId->Handler ()
addToCart refid = modifyCart (S.insert refid)

removeFromCart::ReferenceId->Handler ()
removeFromCart refid = modifyCart (S.delete refid)

clearCart::Handler ()
clearCart = modifyCart (\_ -> empty)
