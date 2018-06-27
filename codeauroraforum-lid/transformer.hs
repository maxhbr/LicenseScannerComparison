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
import Debug.Trace

data RawFinding
  = RawFinding
  { rfPath :: Text
  , rfLicense :: Text
  } deriving (Show, Eq)

instance CSV.FromNamedRecord RawFinding where
  parseNamedRecord m =
    RawFinding
      <$> m .: "input file path"
      <*> m .: "matched license type"

data Finding
  = Finding
  { path :: Text
  , licenses :: [Text]
  } deriving (Show, Eq)

unRaw :: RawFinding -> Finding
unRaw rf = Finding (rfPath rf) [rfLicense rf]

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

getSourceFileFromDir :: FilePath -> IO FilePath
getSourceFileFromDir dir = do
  input <- fold (ls dir) (F.list)
  return . L.last . L.sort $ L.filter (\ f -> T.extension f == Just "csv") input

main :: IO ()
main = let
    optionsParser :: T.Parser (FilePath, FilePath)
    optionsParser = (,) <$> argPath "sourceDir" "SourceDir"
                 <*> argPath "target" "Target"
  in do
    (sourceDir, target) <- options "Transformer" optionsParser
    sourceFile <- getSourceFileFromDir sourceDir
    content <- TB.strict $ TB.input sourceFile
    let parsed = CSV.decodeByName (BSL.fromStrict content) :: Either String (CSV.Header, V.Vector RawFinding)
    case parsed of
      Right (_, rawFindings) -> do
        let convertedFindings = V.toList . V.map unRaw $ rawFindings
        let filteredFindings = L.filter (\ f -> (Tx.length . Tx.concat $ licenses f) > 0 ) convertedFindings
        let groupedFindings = L.groupBy (\ f1 -> \ f2 -> (path f1 == path f2)) filteredFindings
        let collectedFindings = L.map (\ fs -> let
                                            p = path $ L.head fs
                                            ls = L.concatMap licenses fs
                                          in Finding p ls) groupedFindings
        (writeTextFile target . Tx.decodeUtf8 . convertToCSV . L.sort) collectedFindings
      Left error            -> print error
