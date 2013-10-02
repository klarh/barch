{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Barch.Adaptors where

import Prelude
import Yesod
import Control.Applicative
import Data.Time
import Data.Time.Calendar (fromGregorian)
import qualified Text.BibTeX.Entry as Bib
import qualified Data.Map.Lazy as M
import Data.Text as T
import Text.Parsec
import Yesod.Markdown

import Model
import qualified Barch.QueryParser as Q

-- touchReference: update the update time of a Reference to now
touchReference::MonadIO m=>Reference->m Reference
touchReference ref = do
  time <- liftIO getCurrentTime
  return $ ref {referenceLastModified = time}

-- touchMaybeReference: update the update time of a Maybe Reference to now
touchMaybeReference::MonadIO m=>Maybe Reference->m (Maybe Reference)
touchMaybeReference mref = do
  case mref of
    Nothing -> return Nothing
    Just ref' -> do
                 ref <- touchReference ref'
                 return $ Just ref

entry2Reference::[Text]->Markdown->Bib.T->Reference
entry2Reference tags notes entry = (entry2Reference' tags notes entry) $ UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)

entry2Reference'::[Text]->Markdown->Bib.T->(UTCTime->Reference)
entry2Reference' tags notes (Bib.Cons typ ident fields) =
  Reference (pack typ) (pack ident) fields' (M.keys fields') tags notes
  where
    fields' = (M.map pack . M.mapKeys pack) $ M.fromList fields

reference2Entry::Reference->Bib.T
reference2Entry (Reference typ ident fields _ _ _ _) =
  Bib.Cons (unpack typ) (unpack ident) fields'
  where
    fields' = M.toList $ (M.map unpack . M.mapKeys unpack) fields

text2Tags::Text->[Text]
text2Tags t = Q.unTag <$> (either (\_->[]) id $ parse Q.tags "" t)
--text2Tags t = Prelude.filter (not . T.null) $ T.strip <$> T.splitOn (" "::Text) t

-- wrapTag: wrap a tag in quotation marks if it contains any spaces
wrapTag::Text->Text
wrapTag x
  | T.any (==' ') x = T.snoc ('"' `T.cons` x) '"'
  | otherwise = x

tags2Text::[Text]->Text
tags2Text xs = T.intercalate " " (wrapTag <$> xs)

parseBibUrl::(Text, Text)->Either (Text, Text) (Text, Html)
parseBibUrl (key, val) = case (T.toLower key) of
  "url" -> Right $ (key, [shamlet|<a href="#{val}">#{val}</a>|])
--  "eprint" -> Right $ (key, [shamlet|<a href="#{val}">#{val}</a>|])
  "doi" -> Right $ (key, [shamlet|<a href="doi://#{val}">#{val}</a>|])
  _ -> Left (key, val)
