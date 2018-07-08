#!/usr/bin/env stack
{- stack script --resolver=lts-11.9
 --package=turtle
 --package=text
 --package=vector
 --package=either
 --package=cassava
 --package=word8
 --package=bytestring
 --package=foldl
 --package=containers
 -}
{-# OPTIONS_GHC -threaded        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Csv as CSV
import Data.Word8 as W8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Control.Foldl as F
import qualified Data.Map as Map

data RawFinding
  = RawFinding
  { rfPath :: Text
  , rfLicId :: Text
  , rfLicShortname :: Text
  , rfSPDXId :: Text
  } deriving (Show, Eq)

instance CSV.FromNamedRecord RawFinding where
  parseNamedRecord m =
    RawFinding
      <$> m .: "Resource"
      <*> m .: "license__key"
      <*> m .: "license__short_name"
      <*> m .: "license__spdx_license_key"

data Finding
  = Finding
  { path :: Text
  , licenses :: [Text]
  , origLicenses :: [Text]
  } deriving (Show, Eq)

unRaw :: RawFinding -> Finding
unRaw rf = let
    getLicIdFromRf rf@RawFinding{rfSPDXId = ""} = rfLicShortname rf
    getLicIdFromRf rf                           = rfSPDXId rf
  in Finding (rfPath rf) [getLicIdFromRf rf] [getLicIdFromRf rf]

instance Ord Finding where
  f <= f' = (path f) <= (path f')

convertToCSV :: [Finding] -> BS.ByteString
convertToCSV = let
    options = CSV.defaultEncodeOptions
      { CSV.encUseCrLf = False
      , CSV.encQuoting = CSV.QuoteMinimal }
    toTuples :: Finding -> (Text, Text, Text)
    toTuples f = (path f, Tx.intercalate ";" ((L.sort . licenses) f), Tx.intercalate ";" ((L.sort . origLicenses) f))
  in BSL.toStrict . (CSV.encodeWith options) . L.map toTuples

getSourceFileFromDir :: FilePath -> FilePath
getSourceFileFromDir = (</> "output.csv")

rewriteMap :: Map.Map Text [Text]
rewriteMap = Map.fromList
  [ ("Public Domain", ["Public-domain"])
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
    content <- TB.strict $ TB.input (getSourceFileFromDir sourceDir)
    let parsed = CSV.decodeByName (BSL.fromStrict content) :: Either String (CSV.Header, V.Vector RawFinding)
    case parsed of
      Right (_, rawFindings) -> do
        let convertedFindings = V.toList . V.map unRaw $ rawFindings
        let filteredFindings = L.filter (\ f -> (Tx.length . Tx.concat $ licenses f) > 0 ) convertedFindings
        let groupedFindings = L.groupBy (\ f1 -> \ f2 -> (path f1 == path f2)) filteredFindings
        let collectedFindings = L.map (\ fs -> let
                                            p = path $ L.head fs
                                            ls = L.concatMap licenses fs
                                            origLs = L.concatMap origLicenses fs
                                          in Finding p ls origLs) groupedFindings
        (writeTextFile target . Tx.decodeUtf8 . convertToCSV . L.sort) collectedFindings
      Left error            -> print error
