{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Adaptors where

import Prelude
import Control.Applicative
import qualified Text.BibTeX.Entry as Bib
import qualified Data.Map.Lazy as M
import Data.Text as T
import Yesod.Markdown

import Model

entry2Reference::[Text]->Markdown->Bib.T->Reference
entry2Reference tags notes (Bib.Cons typ ident fields) =
  Reference (pack typ) (pack ident) fields' tags notes
  where
    fields' = (M.map pack . M.mapKeys pack) $ M.fromList fields

reference2Entry::Reference->Bib.T
reference2Entry (Reference typ ident fields _ _) =
  Bib.Cons (unpack typ) (unpack ident) fields'
  where
    fields' = M.toList $ (M.map unpack . M.mapKeys unpack) fields

text2Tags::Text->[Text]
text2Tags t = Prelude.filter (not . T.null) $ T.strip <$> T.splitOn (" "::Text) t

tags2Text::[Text]->Text
tags2Text = T.intercalate " "
