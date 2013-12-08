{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Search where

import Yesod.Markdown
import Data.Maybe
import Database.Persist.MongoDB
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Parsec

import Import
import qualified Barch.QueryParser as Q
import Barch.Widgets

-- | take a list of query elements and return the database query to
-- run
query2DBFilter::[Q.Elt]->[Filter Reference]
query2DBFilter =
  mapMaybe f
  where
    f (Q.Field t _) = Just $ multiEq ReferenceFieldKeys t
    f (Q.Tag t) = Just $ multiEq ReferenceTags t
    f _ = Nothing

-- | take a list of query elements and return the function to filter
-- the results of the databse query by; assumes that tags have already
-- been checked, for example
query2Filter::[Q.Elt]->(Entity Reference->Bool)
query2Filter q =
  \ref -> foldl (&&) True $ pure f <*> q <*> pure ref
  where
    f (Q.Field k v) (Entity _ ref) = T.isInfixOf v ((referenceFields ref) M.! k)
    f (Q.Plain t) (Entity _ ref) = t `occursIn` ref
    f (Q.Tag _) _ = True

-- | return true if the given text occurs in any fields of a reference
occursIn::Text->Reference->Bool
occursIn txt (Reference typ ident fields _ tags notes _) =
  inList [typ, ident, unMarkdown notes] || inFields || inTags
  where
    inList = any (T.isInfixOf txt . T.toLower)
    inFields = (inList . M.keys) fields || (inList . M.elems) fields
    inTags = inList tags

getSearchR::Text->Handler Html
getSearchR query = do
    let submission = ""
        handlerName = "getSearchR" :: Text
        fieldQuery = parse Q.line "" query
        queryDBFilter = case fieldQuery of
          Right parsed -> query2DBFilter parsed
          Left _ -> []
        queryFilter = case fieldQuery of
          Right parsed -> query2Filter parsed
          Left _ -> \_ -> False

    possibleMatches <- runDB $ selectList queryDBFilter []

    let matches = filter queryFilter possibleMatches
        wasQuery = any (not . T.null) [query, submission]
        anyMatches = not . null $ matches

    ((result, formWidget), formEnctype) <- runFormPost $ searchReferenceForm

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch: Search"
        $(widgetFile "search")

postSearchR::Text->Handler Html
postSearchR query = do
    ((result, formWidget), formEnctype) <- runFormPost $ searchReferenceForm
    let submission = case result of
          FormSuccess res -> T.toLower res
          _ -> ""
        fieldQuery = parse Q.line "" submission
        queryDBFilter = case fieldQuery of
          Right parsed -> query2DBFilter parsed
          Left _ -> []
        queryFilter = case fieldQuery of
          Right parsed -> query2Filter parsed
          Left _ -> \_ -> False

    possibleMatches <- runDB $ selectList queryDBFilter [Desc ReferenceLastModified]

    let matches = filter queryFilter possibleMatches
        handlerName = "postSearchR" :: Text
        wasQuery = any (not . T.null) [query, submission]
        anyMatches = not . null $ matches

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch: Search"
        $(widgetFile "search")

searchReferenceForm::Form Text
searchReferenceForm = renderDivs $ areq (searchField True) "Search Query" Nothing
