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
