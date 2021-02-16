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
module Snowflake.Tab.Scenes
(tabScenes
) where

import           Data.Maybe
import           Text.Pandoc.Definition
import           Text.Pandoc.Shared

import           Snowflake.Content
import           Snowflake.Database
import           Snowflake.Menu


tabScenes :: [Element] -> MenuContent
tabScenes db = do
  fourPage <- findSection db ["design", "four-page-summary"]
  scenes <- findSection db ["design", "scenes"]
  return [
    buildTable2
      (rows fourPage scenes)
    ]
  where
  rows fourPage scenes =
    map mkrow $ zip (filter (not.isBlk) $ secContent fourPage)
      (filter (not.isBlk) $ secContent scenes)

  mkrow :: (Element,Element) -> [[Block]]
  mkrow (fp@Sec {},sc@Sec {}) =
    [concatMap flattenElement $ secContent fp
    ,concatMap flattenElement $ secContent sc]
  mkrow _ = []

