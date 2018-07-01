#!/usr/bin/env stack
{- stack script --resolver=lts-11.9
 --package=turtle
 --package=text
 --package=aeson
 --package=vector
 --package=cassava
 --package=bytestring
 --package=foldl
 --package=system-filepath
 --package=raw-strings-qq
 --package=containers
 -}
{-# OPTIONS_GHC -threaded      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE QuasiQuotes       #-}
import Prelude hiding (FilePath)
import qualified Prelude as P
import Turtle as T
import Turtle.Bytes as TB
import qualified Data.Csv as CSV
import qualified Control.Foldl as F
import Control.Monad
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Filesystem.Path as FP
import Filesystem.Path.CurrentOS as FP
import Text.RawString.QQ
import qualified Data.Map as Map


import Data.Aeson as A
import Data.Aeson.Types as A
import Data.Text as Tx
import Data.Text.IO as Tx
import Data.Text.Encoding as Tx
import Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL


data Finding
  = Finding
  { path :: FilePath
  , licenses :: [Text]
  , comment :: Text
  } deriving (Show, Eq, Generic)

instance CSV.FromRecord Finding where
  parseRecord v | V.length v == 3 = Finding <$> (fmap FP.fromText $ v `CSV.index` 0)
                                            <*> (fmap (Tx.splitOn ";") $ v `CSV.index` 1)
                                            <*> v `CSV.index` 2
                | otherwise       = mzero

listFiles :: FilePath -> IO [FilePath]
listFiles resultDir = fold (ls resultDir) F.list

listCsvs :: FilePath -> IO [FilePath]
listCsvs = (liftM (L.filter (`hasExtension` "csv"))) . listFiles

getCsvsContent :: FilePath -> IO [(String, [Finding])]
getCsvsContent resultDir = let
    getCsvContent :: FilePath -> IO (String, V.Vector Finding)
    getCsvContent csv = do
      csvData <- fmap BSL.fromStrict $ fold (TB.input csv) (F.foldMap id id)
      case (CSV.decode CSV.NoHeader csvData) :: Either String (V.Vector Finding) of
        Left err -> do
          fail err
        Right results -> return (FP.encodeString $ FP.basename csv, results)
  in do
    csvs <- listCsvs resultDir
    csvsContensPre <- mapM getCsvContent csvs
    return $ L.map (\(s, fvector) -> (s, V.toList fvector)) csvsContensPre

loadFileList :: FilePath -> IO [FilePath]
loadFileList resultDir = let
    fileListFile = resultDir </> "_fileList"
  in fmap (L.map (FP.fromText . lineToText)) $ fold (T.input fileListFile) F.list


transposeCsvsContent :: [FilePath] -> [(String, [Finding])] -> Map.Map FilePath [(String, [Finding])]
transposeCsvsContent fileList csvsContent = let
    getMatchingFindings :: FilePath -> [(String, [Finding])]
    getMatchingFindings fp = L.map (\ (s, fs) -> (s, L.filter (\ f -> (path f) == fp) fs)) csvsContent
    transposeOne :: FilePath -> (FilePath, [(String, [Finding])])
    transposeOne fp = (fp, Map.fromList getMatchingFindings fp)
  in Map.fromList $ L.map transposeOne fileList

templateStart :: Text
templateStart = [r|
<!DOCTYPE html>
<html>
<head>
<title>LicenseScannerComparison</title>
<link rel="stylesheet" type="text/css" href="https://cdn.datatables.net/1.10.19/css/jquery.dataTables.css">
</head>
<body>
<table id="maintable" class="display">
|]
templateEnd :: Text
templateEnd = [r|
</table>
<script type="text/javascript" charset="utf8" src="https://code.jquery.com/jquery-3.3.1.min.js"></script>
<script type="text/javascript" charset="utf8" src="https://cdn.datatables.net/1.10.19/js/jquery.dataTables.js"></script>
<script type="text/javascript">
$(document).ready( function () {
    $('#maintable').DataTable();
} );
</script>
</body>
</html>|]

renderHeader :: [String] -> Text
renderHeader scanners = "<thead><tr><th>Path</th><th>" `Tx.append` (Tx.intercalate "</th><th>" $ L.map Tx.pack scanners) `Tx.append` "</th></thead>"
renderRows :: [String] -> Map.Map FilePath [(String, [Finding])] -> Text
renderRows scanners = let
    renderRowFun :: Text -> FilePath -> [(String, [Finding])] -> Text
    renderRowFun old path findings = undefined
  in Map.foldlWithKey renderRowFun ""

generateHtml :: [String] -> Map.Map FilePath [(String, [Finding])] -> Text
generateHtml scanners resultData = Tx.unlines $ [templateStart, renderHeader scanners, "<tbody>", renderRows scanners resultData, "<tbody>", templateEnd]

main :: IO ()
main = let
    optionsParser :: T.Parser FilePath
    optionsParser = argPath "sourceDir" "SourceDir"
  in do
    resultDir <- options "Page generator" optionsParser
    csvsContent <- getCsvsContent resultDir
    fileList <- loadFileList resultDir
    let resultData = transposeCsvsContent fileList csvsContent
    let scanners = L.map (\(s, _) -> s) csvsContent
    let html = generateHtml scanners resultData
    Tx.putStrLn html

