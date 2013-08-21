{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import qualified Data.Map as M
import qualified Data.Text as T
import Import
import Barch.Adaptors

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    citations <- runDB $ selectList [] [Desc ReferenceLastModified, LimitTo 10]
--    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Barch Homepage"
        $(widgetFile "homepage")

-- postHomeR :: Handler Html
-- postHomeR = do
--     citations <- runDB $ selectList [] [Asc ReferenceId]
--     ((result, formWidget), formEnctype) <- runFormPost sampleForm
--     let handlerName = "postHomeR" :: Text
--         submission = case result of
--             FormSuccess res -> Just res
--             _ -> Nothing

--     defaultLayout $ do
--         aDomId <- newIdent
--         setTitle "Barch Homepage"
--         $(widgetFile "homepage")

-- sampleForm :: Form (FileInfo, Text)
-- sampleForm = renderDivs $ (,)
--     <$> fileAFormReq "Choose a file"
--     <*> areq textField "What's on the file?" Nothing
