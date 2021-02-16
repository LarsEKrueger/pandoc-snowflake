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
module Snowflake.Content
( titledSection
, inlinesToString
, stripSingleParagraph
, buildTable2
) where

import           Text.Pandoc.Definition
import           Text.Pandoc.Shared

import           Snowflake.Database

import Data.List as L
import Data.Text as T

titledSection :: Int -> Element -> Text -> [Block]
titledSection _ (Blk _) _ = []
titledSection depth (Sec _ _ _ _ cont) headline =
  Header depth ("",[],[]) [Str headline]
  : L.concatMap flattenElement cont

inlinesToString :: [Inline] -> Text
inlinesToString = T.concat . L.map itos

itos :: Inline -> Text
itos (Str s)         = s
itos (Emph e)        = inlinesToString e
itos (Strong s)      = inlinesToString s
itos (Strikeout s)   = inlinesToString s
itos (Superscript s) = inlinesToString s
itos (Subscript s)   = inlinesToString s
itos (SmallCaps s)   = inlinesToString s
itos (Span _ s)      = inlinesToString s
itos Space           = " "
itos SoftBreak       = " "
itos LineBreak       = " "
itos _               = ""

stripSingleParagraph :: [Block] -> [Block]
stripSingleParagraph [Para x] = [Plain x]
stripSingleParagraph x        = x

buildTable2 :: [[[Block]]] -> Block
buildTable2 cellRows =
  Table
       nullAttr
       (Caption Nothing [])
       [(AlignLeft,ColWidthDefault),(AlignLeft,ColWidthDefault)]
       (TableHead nullAttr [])
       [(TableBody nullAttr 0 [] cells)]
       (TableFoot nullAttr [])
  where
  cells = L.map (\cells -> Row nullAttr $ L.map (\cell -> Cell nullAttr AlignDefault 1 1 cell) cells) cellRows
