{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Delete where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Parsec
import qualified Text.BibTeX.Parse as BibP
--import qualified Text.BibTeX.Entry as BibE
import qualified Text.BibTeX.Format as BibF
import Yesod.Markdown

import Import
import Barch.Adaptors
import Barch.UploadUtils
import Handler.Edit

-- This is a handler function for the GET request method on the DeleteR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getDeleteR :: ReferenceId->Handler Html
getDeleteR refid = do
    dbRef <- runDB $ get refid

    (formWidget, formEnctype) <- generateFormPost $ editReferenceForm dbRef
    let submission = Nothing :: Maybe Text
        handlerName = "getDeleteR" :: Text
        fieldText = "" :: Text
        parsed = Nothing
        reference = Nothing

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch: Delete"
        $(widgetFile "delete")

postDeleteR :: ReferenceId->Handler Html
postDeleteR refid = do
    dbRef <- runDB $ get refid

    let handlerName = "postDeleteR" :: Text
        reference = Nothing

    (editFormWidget, editFormEnctype) <- generateFormPost $ editReferenceForm dbRef
    (uploadFileFormWidget, uploadFileFormEnctype) <- generateFormPost $ addFileForm

    files <- referenceFiles refid
    _ <- mapM deleteFile (entityKey <$> files)
    _ <- runDB $ delete refid

    defaultLayout $ do
      aDomId <- newIdent
      setTitle "Barch: Delete"
      $(widgetFile "edit")
