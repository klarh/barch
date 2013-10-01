{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Browse where

import qualified Data.Map as M
import qualified Data.Text as T
import Import
import Barch.Adaptors
import Barch.Widgets (shortReferenceView)

pageLimit::Int
pageLimit = 30

getBrowseR::Int->Handler Html
getBrowseR page = do
    citations <- runDB $ selectList [] [Desc ReferenceLastModified, LimitTo (pageLimit + 1), OffsetBy (pageLimit*page)]
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getBrowseR" :: Text
        moreCitations = (length citations) > pageLimit
        citations' = take pageLimit citations
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch Homepage"
        $(widgetFile "browse")
