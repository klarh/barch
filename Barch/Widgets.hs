{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Barch.Widgets where

import Data.Monoid (mconcat)
import qualified Data.Map as M
import qualified Data.Text as T
import Barch.Adaptors
import Import
import Yesod.Markdown

shortReferenceView::Entity Reference->Widget
shortReferenceView (Entity refid (Reference typ ident fields _ tags _ _)) =
  $(widgetFile "shortReferenceView")

fullReferenceView::Bool->Reference->Widget
fullReferenceView makeLink (Reference typ ident fields _ tags notes _) =
  $(widgetFile "fullReferenceView")

fullReferenceWithFiles::Bool->Entity Reference->Widget
fullReferenceWithFiles makeLink (Entity refid (Reference typ ident fields _ tags notes _)) = do
  dbFiles <- handlerToWidget $ runDB $ selectList [ReferenceFileRef ==. refid] [Asc ReferenceFileVersion]
  let haveFiles = not . null $ dbFiles
  $(widgetFile "fullReferenceWithFiles")
