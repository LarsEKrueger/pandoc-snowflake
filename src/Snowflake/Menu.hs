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
module Snowflake.Menu
( makeMenu
, MenuContent
) where

import           Data.Maybe
import Data.List as L
import Data.Text as T
import Data.String

import           Text.Pandoc.Definition
import           Snowflake.Database

type MenuContent = Maybe [Block]

makeMenu :: Database -> Text -> [(Text,MenuContent)] -> [Block]
makeMenu db menuId sectionGenerators =
  ( Div (menuId,["menubar"],[])
      (L.map fst sections)
    : L.map snd sections
  ) ++
  [ RawBlock (Format "HTML") (T.concat 
      [ "<script>"
      , "var id=localStorage.getItem('"
      , menuId
      , "');"
      , "if (id) {"
      , "selectMenu('"
      , menuId
      , "',id);"
      , "}</script>"
      ])
  ]
  where
  sections = L.map (mkSection menuId db) $ L.zip [0..] sectionGenerators

mkSection :: Text -> Database -> (Int,(Text,MenuContent)) -> (Block,Block)
mkSection id db (ind,(headline,content)) =
  ( RawBlock (Format "HTML") menuItemMarkup
  , Div ( divId, [ divClass ], []) secCont
  )
  where
  indStr = fromString $ show ind
  divId = T.concat [ id, ".", indStr]
  divClass = if ind == 0 then "bodyshow" else "bodyhide"
  spanClass = if ind == 0 then "select" else ""
  menuItemMarkup = T.concat
    [ "<span class=\""
    , spanClass
    , "\" onclick=\"selectMenu('"
    , id
    , "',"
    , indStr
    , ")\">"
    , headline
    , "</span>"
    ]
  secCont = fromMaybe [Plain [Str "content generation failure"]] content
