#!/usr/bin/env stack
{- stack script --resolver=lts-11.9
 --package=turtle
 --package=text
 --package=aeson
 --package=vector
 --package=either
 --package=cassava
 --package=word8
 --package=bytestring
 --package=containers
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
import Data.Aeson as A
import Data.Aeson.Types as A
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
import qualified Data.Map as Map

data RawFinding
  = RawFinding
  { rfFiles :: [Text]
  , rfTag :: Text
  } deriving Show
data Finding
  = Finding
  { path :: Text
  , licenses :: [Text]
  , origLicenses :: [Text]
  } deriving (Show, Eq)

instance Ord Finding where
  f <= f' = (path f) <= (path f')

unRaw :: RawFinding -> [Finding]
unRaw rf = L.map (\ f -> Finding f [rfTag rf] [rfTag rf]) (rfFiles rf)

instance FromJSON RawFinding where
  parseJSON = withObject "RawFinding" $
    \v -> RawFinding
            <$> v .: "files"
            <*> v .: "tag"

parseJsonSource :: FilePath -> IO (Either String [Finding])
parseJsonSource source = do
  content <- TB.strict $ TB.input source
  return $ (mapRight ((L.concatMap unRaw) :: [RawFinding] -> [Finding]) . A.eitherDecodeStrict') content

convertToCSV :: [Finding] -> BS.ByteString
convertToCSV = let
    options = CSV.defaultEncodeOptions
      { CSV.encUseCrLf = False
      , CSV.encQuoting = CSV.QuoteMinimal }
    toTuples :: Finding -> (Text, Text, Text)
    toTuples f = (path f, Tx.intercalate ";" ((L.sort . licenses) f), Tx.intercalate ";" ((L.sort . origLicenses) f))
  in BSL.toStrict . (CSV.encodeWith options) . L.map toTuples

getSourceFileFromDir :: FilePath -> FilePath
getSourceFileFromDir = (</> "output.json")

rewriteMap :: Map.Map Text [Text]
rewriteMap = Map.fromList
  [ ("none",[])
  , ("Zlib_fileref", ["Zlib"]), ("Zlib_ref1", ["Zlib"]), ("Zlib_ref2", ["Zlib"]), ("Zlib_ref3", ["Zlib"]), ("gzlog.h_fileref", ["Zlib"])
  , ("PD", ["Public-domain"])
  , ("BSL-1.0_urlref", ["BSL-1.0"])
  , ("Libpng_2", ["Libpng"])
  ]

rewriteFindings :: Map.Map Text [Text] -> [Finding] -> [Finding]
rewriteFindings map = let
    rewriteLicense lic = case lic `Map.lookup` map of
      Just newLics -> newLics
      otherwise    -> [lic]
    rewriteFinding finding@Finding{ licenses = lics } = finding { licenses = L.concatMap rewriteLicense lics }
  in L.map rewriteFinding

main :: IO ()
main = let
    optionsParser :: T.Parser (FilePath, FilePath)
    optionsParser = (,) <$> argPath "sourceDir" "SourceDir"
                 <*> argPath "target" "Target"
  in do
    (sourceDir, target) <- options "Transformer" optionsParser
    parsed <- (parseJsonSource . getSourceFileFromDir) sourceDir
    case parsed of
      Left error -> print error
      Right v    -> (writeTextFile target . Tx.decodeUtf8 . convertToCSV . L.sort . (rewriteFindings rewriteMap)) v
