{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Delete where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Error
import qualified Text.BibTeX.Parse as BibP
import qualified Text.BibTeX.Format as BibF
import Yesod.Markdown

import Import
import Barch.Adaptors
import Barch.UploadUtils
import Barch.Widgets (fullReferenceView)
import Handler.Edit

getDeleteR :: ReferenceId->Handler Html
getDeleteR refid = do
    dbRef <- runDB $ get refid

    (formWidget, formEnctype) <- generateFormPost $ editReferenceForm dbRef
    let submission = Nothing :: Maybe Text
        handlerName = "getDeleteR" :: Text
        fieldText = "" :: Text
        parsed = Nothing
        reference = Nothing
        parseErrors = []
        haveErrors = False

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch: Delete"
        $(widgetFile "delete")

postDeleteR :: ReferenceId->Handler Html
postDeleteR refid = do
    dbRef <- runDB $ get refid

    let handlerName = "postDeleteR" :: Text
        reference = Nothing
        parseErrors = []
        haveErrors = False

    (editFormWidget, editFormEnctype) <- generateFormPost $ editReferenceForm dbRef

    files <- referenceFiles refid
    _ <- mapM deleteFile (entityKey <$> files)
    _ <- runDB $ delete refid

    defaultLayout $ do
      aDomId <- newIdent
      setTitle "Barch: Delete"
      $(widgetFile "edit")
