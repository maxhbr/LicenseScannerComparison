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
  , rfLicense :: Text
  } deriving (Show, Eq)

instance CSV.FromRecord RawFinding where
  parseRecord v | V.length v >= 2 = RawFinding <$> v .! 0 <*> v .! 1
                | otherwise       = mzero

data Finding
  = Finding
  { path :: Text
  , licenses :: [Text]
  } deriving (Show, Eq)

unRaw :: RawFinding -> Finding
unRaw rf = Finding (rfPath rf) (Tx.splitOn "," $ rfLicense rf)

instance Ord Finding where
  f <= f' = (path f) <= (path f')

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

rewriteMap :: Map.Map Text [Text]
rewriteMap = Map.fromList
  [ ("NONE",[])
  , ("GPLv1", ["GPL-1.0-only"]), ("GPLv1+", ["GPL-1.0-or-later"])
  , ("GPLv2", ["GPL-2.0-only"]), ("GPLv2+", ["GPL-2.0-or-later"])
  , ("GPLv3", ["GPL-3.0-only"]), ("GPLv3+", ["GPL-3.0-or-later"])
  , ("LesserGPLv2", ["LGPL-2.0-only"]), ("LesserGPLv2+", ["LGPL-2.0-or-later"])
  , ("LesserGPLv2.1", ["LGPL-2.1-only"]), ("LesserGPLv2.1+", ["LGPL-2.1-or-later"])
  , ("LesserGPLv3", ["LGPL-3.0-only"]), ("LesserGPLv3+", ["LGPL-3.0-or-later"])
  , ("AGPLv1", ["AGPL-1.0-only"])
  , ("AGPLv3", ["AGPL-3.0-only"])
  , ("GPLnoVersion", ["GPL-1.0-only", "GPL-1.0-or-later", "GPL-2.0-only", "GPL-2.0-or-later", "GPL-3.0-only", "GPL-3.0-or-later"])
  , ("Apache-2", ["Apache-2.0"])
  , ("spdxMIT", ["MIT"]), ("spdxBSD2", ["BSD2"]), ("spdxBSD3", ["BSD3"]), ("spdxBSD4", ["BSD4"])
  , ("MITX11", ["X11"])
  , ("BeerWareVer42", ["Beerware"])
  , ("ZLIB", ["Zlib"])
  , ("postgresql", ["PostgreSQL"])
  , ("NPLv1_0", ["NPL-1.1"])
  , ("MPLv1_1", ["MPL-1.1"])
  , ("boost-1", ["BSL-1.0"])
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
    decodeOptions = CSV.defaultDecodeOptions {
      decDelimiter = W8._semicolon
    }
  in do
    (sourceDir, target) <- options "Transformer" optionsParser
    content <- TB.strict $ TB.input (getSourceFileFromDir sourceDir)
    let parsed = CSV.decodeWith decodeOptions CSV.NoHeader (BSL.fromStrict content) :: Either String (V.Vector RawFinding)
    case parsed of
      Right rawFindings -> do
        let convertedFindings = V.toList . V.map unRaw $ rawFindings
        let rewritenFindings = rewriteFindings rewriteMap convertedFindings
        let filteredFindings = L.filter (\ f -> (Tx.length . Tx.concat $ licenses f) > 0 ) rewritenFindings
        let groupedFindings = L.groupBy (\ f1 -> \ f2 -> (path f1 == path f2)) filteredFindings
        let collectedFindings = L.map (\ fs -> let
                                            p = path $ L.head fs
                                            ls = L.concatMap licenses fs
                                          in Finding p ls) groupedFindings
        (writeTextFile target . Tx.decodeUtf8 . convertToCSV . L.sort) collectedFindings
      Left error            -> print error
