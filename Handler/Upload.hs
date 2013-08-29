{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Upload where

import Data.Maybe
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Error
import qualified Text.BibTeX.Parse as BibP
import qualified Text.BibTeX.Entry as BibE
import Yesod.Markdown

import Import
import Barch.Adaptors
import Barch.UploadUtils
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
    (editFormWidget, editFormEnctype) <- generateFormPost $ editReferenceForm Nothing
    let submission = Nothing :: Maybe Text
        handlerName = "getUploadR" :: Text
        fieldText = "" :: Text
        parsed = []
        errors = []
        haveErrors = False
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch: Upload"
        $(widgetFile "upload")

postUploadR :: Handler Html
postUploadR = do
    ((editResult, editFormWidget), editFormEnctype) <- runFormPost $ editReferenceForm Nothing

    let handlerName = "postUploadR" :: Text
        submission = case editResult of
            FormSuccess res -> Just res
            _ -> Nothing
        (bibText, tagsText, notes, fVersion, fileRes) = case submission of
            Nothing -> ("" :: Text, ""::Text, Markdown "", Nothing, Nothing)
            Just (b, t, n, v, f) -> (unTextarea b, fromMaybe "" t, fromMaybe (Markdown "") n, v, f)
        parseRes = parse (BibP.skippingLeadingSpace BibP.file) "" (T.unpack bibText)
        parsed = case parseRes of
            Left err -> []
            Right xs -> xs
        errors = case parseRes of
          Left err -> errorMessages err
          _ -> []
        haveErrors = not . null $ errors
        references' = entry2Reference (text2Tags tagsText) notes <$> parsed
        fileSubmission = case fileRes of
          Just file -> Just (fromMaybe (fileName file) fVersion, file)
          _ -> Nothing


    references <- mapM touchReference references'

    refids <- runDB $ insertMany references

    _ <- case fileSubmission of
      Nothing -> return Nothing
      Just (version, file) -> case refids of
        [] -> return Nothing
        (refid:_) -> do
                       fileId <- insertFile refid version file
                       return $ Just fileId

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch: Upload"
        $(widgetFile "upload")

-- uploadRecordForm::Form (Textarea, Maybe Text, Markdown)
-- uploadRecordForm = renderDivs $ (,,)
--   <$> areq textareaField "BibTeX Record" Nothing
--   <*> aopt textField "Tags" Nothing
--   <*> areq markdownField "Notes" Nothing
