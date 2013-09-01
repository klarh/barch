{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.DeleteUploadedFile where

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
getDeleteUploadedFileR :: ReferenceFileId->Handler Html
getDeleteUploadedFileR refid = do
    dbRef <- runDB $ get refid

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch: Delete Upload"
        $(widgetFile "deleteUploadedFile")

postDeleteUploadedFileR :: ReferenceFileId->Handler Html
postDeleteUploadedFileR refid = do
    _ <- deleteFile refid

    defaultLayout $ do
      aDomId <- newIdent
      setTitle "Barch: Delete Upload"
      redirect $ HomeR 0
