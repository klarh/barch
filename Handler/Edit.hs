{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Edit where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Error
import qualified Text.BibTeX.Parse as BibP
--import qualified Text.BibTeX.Entry as BibE
import qualified Text.BibTeX.Format as BibF
import Yesod.Markdown

import Import
import Barch.Adaptors
import Barch.UploadUtils

-- This is a handler function for the GET request method on the EditR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getEditR :: ReferenceId->Handler Html
getEditR refid = do
    dbRef <- runDB $ get refid

    (editFormWidget, editFormEnctype) <- generateFormPost $ editReferenceForm dbRef

    let submission = Nothing :: Maybe Text
        handlerName = "getEditR" :: Text
        fieldText = "" :: Text
        parsed = Nothing
        reference = Nothing
        parseErrors = []
        haveErrors = False
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch: Edit"
        $(widgetFile "edit")

postEditR :: ReferenceId->Handler Html
postEditR refid = do
    dbRef <- runDB $ get refid

    ((editResult, editFormWidget), editFormEnctype) <- runFormPost $ editReferenceForm dbRef

    let handlerName = "postEditR" :: Text
        submission = case editResult of
            FormSuccess res -> Just res
            _ -> Nothing
        (bibText, tagsText, notes, fVersion, fileRes) = case submission of
            Nothing -> ("" :: Text, ""::Text, Markdown "", Nothing, Nothing)
            Just (b, t, n, v, f) -> (unTextarea b, fromMaybe "" t, fromMaybe (Markdown "") n, v, f)
        parseRes = parse (BibP.skippingLeadingSpace BibP.file) "" (T.unpack bibText)
        parsed = case parseRes of
            Left _ -> Nothing
            Right (x:_) -> Just x
            Right [] -> Nothing
        parseErrors = case parseRes of
          Left err -> errorMessages err
          _ -> []
        haveErrors = not . null $ parseErrors
        mergeRef = entry2Reference (text2Tags tagsText) notes
        fileSubmission = case fileRes of
          Just file -> Just (fromMaybe (fileName file) fVersion, file)
          _ -> Nothing

    -- reference <- case reference of
    --   Nothing -> return Nothing
    --   Just ref -> do
    --               ref' <- touchReference ref
    --               return $ Just ref'
    reference <- touchMaybeReference (mergeRef <$> parsed)

    _ <- case fileSubmission of
      Nothing -> return Nothing
      Just (version, file) -> do
                              fileId <- insertFile refid version file
                              return $ Just fileId

    case reference of
      Nothing -> return ()
      Just ref -> runDB $ repsert refid ref

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch: Edit"
        $(widgetFile "edit")

editReferenceForm::Maybe Reference->Form (Textarea, Maybe Text, Maybe Markdown, Maybe Text, Maybe FileInfo)
editReferenceForm ref = renderDivs $ (,,,,)
  <$> areq textareaField "BibTeX Record" (Textarea . T.pack . BibF.entry . reference2Entry <$> ref)
  <*> aopt textField "Tags" (Just . tags2Text . referenceTags <$> ref)
  <*> aopt markdownField "Notes" (Just . referenceNotes <$> ref)
  <*> aopt textField "Version" Nothing
  <*> fileAFormOpt "File"
