{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Upload where

import Data.Maybe
import qualified Data.Text as T
import Text.Parsec
import qualified Text.BibTeX.Parse as BibP
import qualified Text.BibTeX.Entry as BibE
import Yesod.Markdown

import Import
import Barch.Adaptors
import Handler.Edit

-- This is a handler function for the GET request method on the UploadR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getUploadR :: Handler Html
getUploadR = do
    (formWidget, formEnctype) <- generateFormPost $ editReferenceForm Nothing
    let submission = Nothing :: Maybe Text
        handlerName = "getUploadR" :: Text
        fieldText = "" :: Text
        parsed = []
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch: Upload"
        $(widgetFile "upload")

postUploadR :: Handler Html
postUploadR = do
    ((result, formWidget), formEnctype) <- runFormPost $ editReferenceForm Nothing
    let handlerName = "postUploadR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
        (bibText, tagsText, notes) = case submission of
            Nothing -> ("" :: Text, ""::Text, Markdown "")
            Just (b, t, n) -> (unTextarea b, fromMaybe "" t, fromMaybe (Markdown "") n)
        parseRes = parse (BibP.skippingLeadingSpace BibP.file) "" (T.unpack bibText)
        parsed = case parseRes of
            Left err -> []
            Right xs -> xs
        references' = entry2Reference (text2Tags tagsText) notes <$> parsed

    references <- mapM touchReference references'

    _ <- runDB $ insertMany references

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch: Upload"
        $(widgetFile "upload")

-- uploadRecordForm::Form (Textarea, Maybe Text, Markdown)
-- uploadRecordForm = renderDivs $ (,,)
--   <$> areq textareaField "BibTeX Record" Nothing
--   <*> aopt textField "Tags" Nothing
--   <*> areq markdownField "Notes" Nothing
