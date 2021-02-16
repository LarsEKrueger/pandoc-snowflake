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
module Snowflake.Tab.Synopsis
( tabCharSyn
) where

import           Data.Maybe
import           Data.List as L
import           Data.Text as T
import           Text.Pandoc.Definition
import           Text.Pandoc.Shared

import           Snowflake.Content
import           Snowflake.Database
import           Snowflake.Menu

tabCharSyn :: Database -> MenuContent
tabCharSyn db = do
  charBase <- findSection db ["design","character-synopsis"]
  return $
    makeMenu db "charSyn" $ mapMaybe contentCharSyn $ secContent charBase

contentCharSyn :: Element -> Maybe (Text,MenuContent)
contentCharSyn (Blk _) = Nothing
contentCharSyn char =
  if headline == "Character"
     then Nothing
     else Just (headline,content)
  where
  headline = inlinesToString $ secHeadline char
  content = Just $ L.concatMap flattenElement $ secContent char


