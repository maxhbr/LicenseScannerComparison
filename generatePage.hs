#!/usr/bin/env stack
{- stack
 --resolver=lts-11.9
 script
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

getCsvContent :: FilePath -> IO (String, V.Vector Finding)
getCsvContent csv = do
  csvData <- fmap BSL.fromStrict $ fold (TB.input csv) (F.foldMap id id)
  case (CSV.decode CSV.NoHeader csvData) :: Either String (V.Vector Finding) of
    Left err -> do
      fail err
    Right results -> return (FP.encodeString $ FP.basename csv, results)

getCsvsContent :: FilePath -> IO [(String, [Finding])]
getCsvsContent resultDir = do
  csvs <- listCsvs resultDir
  csvsContensPre <- mapM getCsvContent csvs
  return $ L.map (\(s, fvector) -> (s, V.toList fvector)) csvsContensPre

rewriteMap :: Map.Map Text [Text]
rewriteMap = Map.fromList
  [ ("GPL-1.0", ["GPL-1.0-only"]), ("GPL-1.0+", ["GPL-1.0-or-later"])
  , ("GPL-2.0", ["GPL-2.0-only"]), ("GPL-2.0+", ["GPL-2.0-or-later"])
  , ("GPL-3.0", ["GPL-3.0-only"]), ("GPL-3.0+", ["GPL-3.0-or-later"])
  , ("LGPL-2.0", ["LGPL-2.0-only"]), ("LGPL-2.0+", ["LGPL-2.0-or-later"])
  , ("LGPL-2.1", ["LGPL-2.1-only"]), ("LGPL-2.1+", ["LGPL-2.1-or-later"])
  , ("LGPL-3.0", ["LGPL-3.0-only"]), ("LGPL-3.0+", ["LGPL-3.0-or-later"])
  , ("AGPL-1.0", ["AGPL-1.0-only"])
  , ("AGPL-3.0", ["AGPL-3.0-only"])
  , ("GPL", ["GPL-1.0-only", "GPL-1.0-or-later", "GPL-2.0-only", "GPL-2.0-or-later", "GPL-3.0-only", "GPL-3.0-or-later"])
  , ("Apache", ["Apache-1.0", "Apache-1.1", "Apache-2.0"])
  , ("GFDL-1.1", ["GFDL-1.1-or-later"]), ("GFDL-1.2", ["GFDL-1.2-or-later"]), ("GFDL-1.3", ["GFDL-1.3-or-later"])]

rewriteFindings :: Map.Map Text [Text] -> [Finding] -> [Finding]
rewriteFindings map = let
    rewriteLicense lic = case lic `Map.lookup` map of
      Just newLics -> newLics
      otherwise    -> [lic]
    rewriteFinding finding@Finding{ licenses = lics } = finding { licenses = L.concatMap rewriteLicense lics }
  in L.map rewriteFinding

rewriteCsvsContent :: [(String, [Finding])] -> [(String, [Finding])]
rewriteCsvsContent = L.map (\(s, fs) -> (s, rewriteFindings rewriteMap fs))

loadFileList :: FilePath -> IO [FilePath]
loadFileList resultDir = let
    fileListFile = resultDir </> "_fileList"
  in fmap (L.map (FP.fromText . lineToText)) $ fold (T.input fileListFile) F.list


transposeCsvsContent :: [FilePath] -> [(String, [Finding])] -> Map.Map FilePath (Map.Map String [Finding])
transposeCsvsContent fileList csvsContent = let
    getMatchingFindings :: FilePath -> [(String, [Finding])]
    getMatchingFindings fp = L.map (\ (s, fs) -> (s, L.filter (\ f -> (path f) == fp) fs)) csvsContent
    transposeOne :: FilePath -> (FilePath, Map.Map String [Finding])
    transposeOne fp = (fp, Map.fromList $ getMatchingFindings fp)
  in Map.fromList $ L.map transposeOne fileList

templateStart :: Text
templateStart = [r|
<!DOCTYPE html>
<html>
<head>
<title>LicenseScannerComparison</title>
<link rel="stylesheet" type="text/css" href="https://cdn.datatables.net/1.10.19/css/jquery.dataTables.css">
<style>
   table { height: 100%; }
   td { height: 100%; }
  .match, .contained, .notContained { width: 100%; height: 100%; display: block; text-align: center; padding: 0.5em 0; }
  .match{ background: lightgreen; }
  .contained{ background: #F5DA81; }
  .intersection{ background: #FE9A2E; }
  .notContained{ background: #FA8072; }
</style>
</head>
<body>
    <a href="../">up</a><br/>
|]
templateEnd :: Text
templateEnd = [r|
<pre id="fileContent" style="width: 92%; display: block; margin: 20px 2%; padding: 20px 2%; border: solid 3px gray;">Preview: works only when run within a server</pre>
<script type="text/javascript" charset="utf8" src="https://code.jquery.com/jquery-3.3.1.min.js"></script>
<script type="text/javascript" charset="utf8" src="https://cdn.datatables.net/1.10.19/js/jquery.dataTables.js"></script>
<script type="text/javascript">
$(document).ready( function () {
    var table = $('#maintable').DataTable({
    });

    $('#maintable tbody').on('click', 'tr', function () {
        var data = table.row( this ).data();
        $.get('./__'+data[0], function(data, status){
            $('#fileContent').text(data);
        });
    } );

    $('a.toggle-vis').on( 'click', function (e) {
        e.preventDefault();

        // Get the column API object
        var column = table.column( $(this).attr('data-column') );

        // Toggle the visibility
        column.visible( ! column.visible() );
    } );
} );
</script>
</body>
</html>|]

renderHeader :: [String] -> Text
renderHeader scanners = "<thead><tr><th>Path</th><th>Expected</th><th>" `Tx.append` (Tx.intercalate "</th><th>" $ L.map Tx.pack scanners) `Tx.append` "</th></tr></thead>"
renderRows :: [String] -> Map.Map FilePath [Text] -> Map.Map FilePath (Map.Map String [Finding]) -> Text
renderRows scanners expected = let
    renderRowFun :: Text -> FilePath -> Map.Map String [Finding] -> Text
    renderRowFun prev path findings = let
        expectedLics = case path `Map.lookup` expected of
          Just lics -> L.sort $ L.nub lics
          otherwise -> []
        pathText = case FP.toText path of
          Left err -> undefined
          Right p  -> p
        getColor actual | Map.null expected                       = ""
                        | actual == expectedLics                  = "match"
                        | expectedLics `L.isSubsequenceOf` actual = "contained"
                        | expectedLics `L.intersect` actual /= [] = "intersection"
                        | otherwise                               = "notContained"
        joinLicenses original lics = (Tx.pack $ "<div class=\"" ++ (getColor $ L.sort $ L.nub lics) ++ "\" title=\"" ++ original ++"\">") `Tx.append` (Tx.intercalate ", " lics) `Tx.append` "</div>"
        mkScannerEntry :: String -> Text
        mkScannerEntry scanner = case Map.lookup scanner findings of
          Just findings -> joinLicenses (L.concatMap (Tx.unpack . comment) findings)  . L.nub $ L.concatMap licenses findings
          otherwise     -> ""
      in Tx.unlines [ prev
                    , "<tr><td>"
                    , Tx.intercalate "</td><td>" ([pathText, Tx.intercalate ", " expectedLics] ++ (L.map mkScannerEntry scanners))
                    , "</td></tr>"
                    ]
  in Map.foldlWithKey renderRowFun ""

renderTable :: [String] -> Map.Map FilePath (Map.Map String [Finding]) -> Map.Map FilePath [Text] -> Text
renderTable scanners resultData expected = Tx.unlines $
  [ "<table id=\"maintable\" class=\"display\">"
  , renderHeader scanners
  , "<tbody>"
  , renderRows scanners expected resultData
  , "</tbody></table>"
  ]

renderToggles :: [String] -> Text
renderToggles scanners = let
    renderToggle (index, name) = Tx.concat
      [ "<a class=\"toggle-vis\" data-column=\""
      , Tx.pack $ show index
      , "\" href=\"#\">"
      , Tx.pack name,"</a>"
      ]
  in "Toggle Scanners:<br/>" `Tx.append` (Tx.intercalate " - " $ L.map renderToggle (L.zip [2..] scanners)) `Tx.append` "<hr>"

generateHtml :: [String] -> Map.Map FilePath (Map.Map String [Finding]) -> Map.Map FilePath [Text] -> Text
generateHtml scanners resultData expected = Tx.unlines
  [ templateStart
  , renderToggles scanners
  , renderTable scanners resultData expected
  , templateEnd
  ]


loadExpectations :: FilePath -> IO (Map.Map FilePath [Text])
loadExpectations fp = do
    expectedFindings <- getCsvContent fp
    return . Map.fromList . L.map (\finding -> (path finding, licenses finding)) . rewriteFindings rewriteMap . V.toList $ (\(_,fs) -> fs) expectedFindings


main :: IO ()
main = let
    optionsParser :: T.Parser (FilePath, FilePath)
    optionsParser = (,) <$> argPath "sourceDir" "SourceDir"
                        <*> argPath "expectationFile" "expectationFile"
  in do
    (resultDir, expectationFile) <- options "Page generator" optionsParser
    csvsContent <- getCsvsContent resultDir
    fileList <- loadFileList resultDir
    expected <- loadExpectations expectationFile
    let resultData = transposeCsvsContent fileList $ rewriteCsvsContent csvsContent
    let scanners = L.sort $ L.map (\(s, _) -> s) csvsContent
    let html = generateHtml scanners resultData expected
    let htmlFile = resultDir </> "index.html"
    T.output htmlFile (return $ T.unsafeTextToLine html)

