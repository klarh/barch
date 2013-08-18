{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Search where

--import qualified Data.Conduit.List as C
import Data.Maybe
import Database.Persist.MongoDB
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Parsec

import Import
--import Barch.Adaptors

type QueryKey = Text
type QueryVal = Text

text2Query::Text->[Text]
text2Query = T.splitOn " "

-- convert the key component a query "path" into a db Filter
queryKeys2Filter::[QueryKey]->[Filter Reference]
queryKeys2Filter = map (multiEq ReferenceFieldKeys)

querySplit::[Text]->[(QueryKey, QueryVal)]
querySplit = map ((\(x, y)->(x, T.tail y)) . (T.breakOn ":"))

queryFilterRef::[(QueryKey, QueryVal)]->Entity Reference->Bool
queryFilterRef q (Entity _ (Reference _ _ fields _ _ _)) =
  all (\(key, val)->T.isInfixOf val (fields M.! key)) q

-- This is a handler function for the GET request method on the SearchR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getSearchR::Text->Handler Html
getSearchR _ = do
    let submission = ""
        handlerName = "getSearchR" :: Text
--        query' = querySplit query
--        queryFilter = queryKeys2Filter $ fst <$> query'
        fieldQuery = []
        matches = []
        records = False

--    possibleMatches <- runDB $ selectList queryFilter []

--    let matches = filter (queryFilterRef query') (possibleMatches)

    ((result, formWidget), formEnctype) <- runFormPost $ searchReferenceForm

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch: Search"
        $(widgetFile "search")

postSearchR::Text->Handler Html
postSearchR _ = do
    ((result, formWidget), formEnctype) <- runFormPost $ searchReferenceForm
    let submission = case result of
          FormSuccess res -> res
          _ -> ""
        fieldQuery = text2Query submission
        query' = querySplit fieldQuery
        queryFilter = queryKeys2Filter $ fst <$> query'

    possibleMatches <- runDB $ selectList queryFilter []

    let matches = filter (queryFilterRef query') (possibleMatches)
        handlerName = "postSearchR" :: Text
        records = not . null $ matches

    liftIO $ print (fieldQuery, query', possibleMatches, matches)

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch: Search"
        $(widgetFile "search")

searchReferenceForm::Form Text
searchReferenceForm = renderDivs $ areq textField "Search Query" Nothing
