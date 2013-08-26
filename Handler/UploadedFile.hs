{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.UploadedFile where

import Import
import Barch.UploadUtils

getUploadedFileR :: ReferenceFileId->Handler TypedContent
getUploadedFileR refid = do
    file <- getFile refid

    case file of
      Nothing -> notFound
      Just f -> sendResponse f

deleteUploadedFileR :: ReferenceFileId->Handler ()
deleteUploadedFileR = deleteFile
