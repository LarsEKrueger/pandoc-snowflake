{-
Copyright (c) 2017 Lars Krueger

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

import           Data.Aeson
import qualified Data.ByteString.Lazy   as B
import           Data.List              as L
import           Text.Pandoc.Definition
import           Text.Pandoc.JSON       as J
import           Text.Pandoc.Shared
import           Text.Pandoc.Walk

import           Snowflake.Content
import           Snowflake.Database
import           Snowflake.Menu
import           Snowflake.Tab.Details
import           Snowflake.Tab.FourPage
import           Snowflake.Tab.Overview
import           Snowflake.Tab.Scenes
import           Snowflake.Tab.Synopsis

makeMainMenu :: [Element] -> [Block]
makeMainMenu db = makeMenu db "mainmenu"
    [ ("Overview", tabOverview db)
    , ("Character Synopsis", tabCharSyn db)
    , ("Four Page Summary", tabFourPage db)
    , ("Character Details", tabCharDet db)
    , ("Scenes", tabScenes db)
    ]

isnotDesign :: Element -> Bool
isnotDesign (Blk _)                = False
isnotDesign (Sec _ _ (id,_,_) _ _) = not $ L.isPrefixOf "design" id

main :: IO ()
main = do
  txt <- B.getContents
  let (Pandoc inMeta inBlocks) = (either error id $ eitherDecode' txt) :: Pandoc
      db = buildDatabase inBlocks
      outBlocks = makeMainMenu db ++
                  concatMap flattenElement notDesignSections
      notDesignSections = filter isnotDesign db

  B.putStr $ encode $ Pandoc inMeta outBlocks
