{-
Copyright (c) 2019 Lars Krueger

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

module Main
where

import           Control.Monad
import           Data.Char
import           Data.List                (unzip6, foldl', intersperse)
import           Paths_pandoc_snowflake
import           System.Console.Haskeline
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import qualified Data.Text as T

inputValue :: String -> IO String
inputValue prefix = do
  value <- runInputT defaultSettings $ getInputLine $ prefix ++ " % "
  case value of
       Nothing -> do
         putStrLn "No value entered. Operation cancelled."
         exitFailure
       Just value -> return value

inputCharacters :: IO [String]
inputCharacters = do
  values <- runInputT defaultSettings $ loop []
  case values of
       [] -> do
         putStrLn "No names entered. Operation cancelled."
         exitFailure
       _ -> return values
  where
  loop :: [String] -> InputT IO [String]
  loop names = do
    value <- getInputLine "Character Name (empty to stop) % "
    case value of
         Nothing   -> return names
         Just ""   -> return names
         Just name -> loop $ name:names

replacePairs :: T.Text -> [(String,String)] -> T.Text
replacePairs = foldl' replacePair
  where
  replacePair :: T.Text -> (String,String) -> T.Text
  replacePair content (search,replace) =
    T.replace (T.pack search) (T.pack replace) content

copyAndReplace :: FilePath -> FilePath -> FilePath -> [(String,String)] -> IO ()
copyAndReplace fromFile fromDir toFile replacements = do
  toFileExists <- doesFileExist toFile
  unless toFileExists $ do
    contents <- readFile $ fromDir </> fromFile
    writeFile toFile $ T.unpack $ replacePairs (T.pack contents) replacements

main :: IO ()
main = do
  lang <- inputValue "Spellcheck Language (e.g. «en»)"
  weirdCaseNameBase <- inputValue "Project Name (a-z only)"
  let nameBase = filter isAlphaNum $ map toLower weirdCaseNameBase
  characters <- inputCharacters
  -- Get path to installed skeleton files
  dataDir <- getDataDir
  let skeletonDir = dataDir </> "skeleton"
  -- Copy include templates
  let replacements = [ ( "@NAMEBASE@", nameBase)
                     , ( "@LANG@", lang)
                     ]
  createDirectoryIfMissing False "include"
  copyAndReplace ("include" </> "design_header.html") skeletonDir ("include" </> "design_header.html") replacements
  copyAndReplace ("include" </> "book_header.html")   skeletonDir ("include" </> "book_header.html")   replacements
  copyAndReplace ("include" </> "epub.css")           skeletonDir ("include" </> "epub.css")           replacements
  copyAndReplace "title.mdwiki"                       skeletonDir "title.mdwiki"                       replacements
  -- Copy character files and build include name lists
  createDirectoryIfMissing False "characters"
  namelists <- forM characters $ \charname -> do
    let lcCharName = map toLower charname
        overview_filename = "characters" </> (lcCharName ++ "_overview.mdwiki")
        details_filename = "characters" </> (lcCharName ++ "_details.mdwiki")
        synopsis_filename = "characters" </> (lcCharName ++ "_synopsis.mdwiki")
        overview_markup = "{{characters/" ++ lcCharName ++ "_overview}}"
        details_markup = "{{characters/" ++ lcCharName ++ "_details}}"
        synopsis_markup = "{{characters/" ++ lcCharName ++ "_synopsis}}"
        charRepl = ( "@CHARACTERNAME@", charname) : replacements
    copyAndReplace "overview.mdwiki" skeletonDir overview_filename  charRepl
    copyAndReplace "details.mdwiki"  skeletonDir details_filename   charRepl
    copyAndReplace "synopsis.mdwiki" skeletonDir synopsis_filename  charRepl
    return ( overview_filename, details_filename, synopsis_filename,
      overview_markup, details_markup, synopsis_markup)

  -- Build the file lists
  let (ov_fns,det_fns,syn_fns,ov_mus,det_mus,syn_mus) = unzip6 $ reverse namelists
      charfiles = concat $ intersperse "\\\n  " ( ov_fns ++ det_fns ++ syn_fns)
      ov_mu = unlines ov_mus
      det_mu = unlines det_mus
      syn_mu = unlines syn_mus
      fullRepl = ("@CHARACTERFILES@", charfiles)
               : ("@OVERVIEWLINKS@", ov_mu)
               : ("@DETAILLINKS@", det_mu)
               : ("@SYNOPSISLINKS@", syn_mu)
               : replacements

  copyAndReplace "design.mdwiki"  skeletonDir "design.mdwiki" fullRepl
  copyAndReplace "book.mdwiki"    skeletonDir "book.mdwiki"   fullRepl
  copyAndReplace "Makefile"       skeletonDir "Makefile"      fullRepl
