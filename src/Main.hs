{-
    Pandoc filter for the Snowflake Writing Method
    Copyright (c) 2017 Lars Krueger

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-}

{-# LANGUAGE OverloadedStrings #-}
module Main
where

import           Data.Aeson
import qualified Data.ByteString.Lazy   as B
import           Data.List              as L
import           Data.Text              as T
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
isnotDesign (Sec _ _ (id,_,_) _ _) = not $ T.isPrefixOf "design" id

main :: IO ()
main = do
  txt <- B.getContents
  let (Pandoc inMeta inBlocks) = (either error id $ eitherDecode' txt) :: Pandoc
      db = buildDatabase inBlocks
      outBlocks = makeMainMenu db ++
                  L.concatMap flattenElement notDesignSections
      notDesignSections = L.filter isnotDesign db

  B.putStr $ encode $ Pandoc inMeta outBlocks
