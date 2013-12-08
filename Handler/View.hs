{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.View where

import qualified Data.Map as M
import qualified Data.Text as T
import Yesod.Markdown

import Import
import Barch.Widgets (fullReferenceWithFiles)

getViewR :: ReferenceId->Handler Html
getViewR refid = do
    dbRef <- runDB $ get refid
    dbFiles <- runDB $ selectList [ReferenceFileRef ==. refid] [Asc ReferenceFileVersion]

    let handlerName = "getViewR" :: Text
        haveFiles = (not . null) dbFiles

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch: View"
        $(widgetFile "view")
