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
module Snowflake.Tab.FourPage
( tabFourPage
) where

import           Data.Maybe
import           Text.Pandoc.Definition
import           Text.Pandoc.Shared

import           Snowflake.Content
import           Snowflake.Database
import           Snowflake.Menu

-- | Make a table where each row is a paragraph from "One Page Summary" and
-- the contents of a sub-section of "Four Page Summary".
tabFourPage :: [Element] -> MenuContent
tabFourPage db = do
  onePage <- findSection db ["design","one-page-summary"]
  fourPage <- findSection db ["design", "four-page-summary"]

  return [
     Table
       nullAttr 
       (Caption Nothing []) 
       [(AlignLeft,ColWidthDefault),(AlignLeft,ColWidthDefault)] 
       (TableHead nullAttr [])
       [(TableBody nullAttr 0 [] $ rows onePage fourPage)]
       (TableFoot nullAttr [])
    ]
  where
  rows onePage fourPage =
    map (\cells -> Row nullAttr cells ) $
    zipWith mkrow (filter isBlk $ secContent onePage)
      (map secContent $ filter (not . isBlk) $ secContent fourPage)

  mkrow :: Element -> [Element]  -> [Cell]
  mkrow  p@(Blk para) cont = [
    Cell nullAttr AlignDefault 1 1 $ flattenElement p,
    Cell nullAttr AlignDefault 1 1 $ concatMap flattenElement cont]
  mkrow _ _                = []

