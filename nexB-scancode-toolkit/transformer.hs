#!/usr/bin/env stack
{- stack script --resolver=lts-11.9
 --package=turtle
 --package=text
 --package=vector
 --package=either
 --package=cassava
 --package=word8
 --package=bytestring
 -}
{-# OPTIONS_GHC -threaded      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE DeriveGeneric     #-}
import Prelude hiding (FilePath)
import qualified Prelude as P
import Turtle as T
import Turtle.Bytes as TB
import GHC.Generics
import qualified Data.Vector as V
import Data.Text as Tx
import Data.Text.IO as Tx
import Data.Text.Encoding as Tx
import Data.Maybe
import Data.Either.Combinators
import Data.List as L
import qualified Data.Csv as CSV
import Data.Word8 as W8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data Lic
  = Lic
  { licenseId :: Text
  } deriving Show

-- Resource,type,name,base_name,extension,size,date,sha1,md5,mime_type,file_type,programming_language,is_binary,is_text,is_archive,is_media,is_source,is_script,files_count,dirs_count,size_count,scan_errors,license__key,license__score,license__short_name,license__category,license__owner,license__homepage_url,license__text_url,license__reference_url,license__spdx_license_key,license__spdx_url,start_line,end_line,matched_rule__identifier,matched_rule__license_expression,matched_rule__licenses,copyright,copyright_holder,author
data RawFinding
  = RawFinding
  { rfFilename :: Text
  , rfDirectory :: Text
  , rfLicenseGuesses :: Maybe [Lic]
  , rfLicenseRoots :: Maybe [Lic]
  } deriving Show
data Finding
  = Finding
  { path :: Text
  , licenses :: [Text]
  } deriving (Show, Eq)

instance Ord Finding where
  f <= f' = (path f) <= (path f')

unRaw :: RawFinding -> Finding
unRaw rf = let
  p = rfDirectory rf `Tx.append` rfFilename rf
  lics = L.nub . P.map licenseId $ (fromMaybe [] $ rfLicenseGuesses rf) ++ (fromMaybe [] $ rfLicenseRoots rf)
  in Finding p lics

instance FromJSON Lic where
  parseJSON = withObject "Lic" $
    \v -> Lic <$> v .: "LicenseId"

instance FromJSON RawFinding where
  parseJSON = withObject "RawFinding" $
    \v -> RawFinding
            <$> v .: "Filename"
            <*> v .: "Directory"
            <*> v .:? "LicenseGuesses"
            <*> v .:? "LicenseRoots"

parseCSVSource :: FilePath -> IO (Either String [Finding])
parseCSVSource source = do
  content <- TB.input source
  return $ (mapRight (P.map unRaw) . CSV.decodeByName) content

convertToCSV :: [Finding] -> BS.ByteString
convertToCSV = let
    options = CSV.defaultEncodeOptions
      { CSV.encUseCrLf = False
      , CSV.encQuoting = CSV.QuoteMinimal }
    toTuples :: Finding -> (Text, Text, Text)
    toTuples f = (path f, Tx.intercalate ";" ((L.sort . licenses) f), "")
  in BSL.toStrict . (CSV.encodeWith options) . L.map toTuples

getSourceFileFromDir :: FilePath -> FilePath
getSourceFileFromDir = (</> "output.csv")

main :: IO ()
main = let
    optionsParser :: T.Parser (FilePath, FilePath)
    optionsParser = (,) <$> argPath "sourceDir" "SourceDir"
                 <*> argPath "target" "Target"
  in do
    (sourceDir, target) <- options "Transformer" optionsParser
    parsed <- (parseCSVSource . getSourceFileFromDir) sourceDir
    case parsed of
      Left error -> print error
      Right v    -> (writeTextFile target . Tx.decodeUtf8 . convertToCSV . L.sort) v
