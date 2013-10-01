{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import qualified Data.Map as M
import qualified Data.Text as T
import Import
import Barch.Adaptors
import Barch.Widgets (shortReferenceView)

pageLimit::Int
pageLimit = 30

getHomeR::Handler Html
getHomeR = do
    citations <- runDB $ selectList [] [Desc ReferenceLastModified, LimitTo (pageLimit + 1), OffsetBy 0]
    let action = "Home" :: Text
        page = 0
        submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
        moreCitations = False
        citations' = take pageLimit citations
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch Homepage"
        $(widgetFile "browse")
