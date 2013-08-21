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
import Yesod.Markdown

import Model

-- entry2Reference::MonadIO m=>[Text]->Markdown->Bib.T->m Reference
-- entry2Reference tags notes entry = do
--   time <- liftIO $ getCurrentTime
--   return $ (entry2Reference' tags notes entry) time
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
text2Tags t = Prelude.filter (not . T.null) $ T.strip <$> T.splitOn (" "::Text) t

tags2Text::[Text]->Text
tags2Text = T.intercalate " "
