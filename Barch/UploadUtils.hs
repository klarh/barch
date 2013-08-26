module Barch.UploadUtils where

import Import
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL (fromChunks, toChunks)
import Data.Conduit
import qualified Data.Conduit.Binary as CB (isolate, take)
import qualified Data.Conduit.List as CL (consume, sequence)
import qualified Data.Text as T
import System.Directory (removeFile)

-- split files into a 256K chunks
chunkSize::Int
chunkSize = 256*1024

-- splitByteString::B.ByteString->[B.ByteString]
-- splitByteString x
--   | length x == 0 = []
--   | otherwise = ((take chunkSize x):(splitByteString $ drop chunkSize x))

-- splitByteStringSink::Sink B.ByteString (ResourceT IO) B.ByteString
-- splitByteStringSink = take chunkSize

--referenceFiles::ReferenceId->[ReferenceFile]
referenceFiles ident = runDB $ selectList [ReferenceFileRef ==. ident] [Asc ReferenceFileVersion]

getFile::ReferenceFileId->Handler (Maybe TypedContent)
getFile fid = do
  fileRecord <- runDB $ get fid
  chunks' <- runDB $ selectList [FileChunkParent ==. fid] [Asc FileChunkIndex]
  let fType = referenceFileType <$> fileRecord
      result = toContent $ BL.fromChunks $ fileChunkContents . entityVal <$> chunks'
  return $ (\typ -> TypedContent typ result) <$> fType

deleteFile::ReferenceFileId->Handler ()
deleteFile fid = do
  _ <- runDB $ deleteWhere [FileChunkParent ==. fid]
  _ <- runDB $ delete fid
  return ()

insertFile::ReferenceId->Text->FileInfo->Handler ReferenceFileId
insertFile refid version file = do
  let refFile = ReferenceFile refid version (encodeUtf8 . fileContentType $ file)
  refFileId <- runDB $ insert refFile
  chunks <- (fileSource file) $= (CL.sequence $ CB.take chunkSize) $$ CL.consume
  _ <- runDB $ insertMany $ zipWith (FileChunk refFileId) [0..] (BS.concat . BL.toChunks <$> chunks)
--  _ <- liftIO $ removeFile $ T.unpack . fileName $ file
  return refFileId
