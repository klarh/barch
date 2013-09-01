{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import qualified Data.Map as M
import qualified Data.Text as T
import Import
import Barch.Adaptors
import Barch.Widgets (shortReferenceView)

getHomeR :: Handler Html
getHomeR = do
    citations <- runDB $ selectList [] [Desc ReferenceLastModified, LimitTo 10]
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch Homepage"
        $(widgetFile "homepage")
