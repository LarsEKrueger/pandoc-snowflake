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
    Table [] [AlignLeft,AlignLeft] [0.5,0.5] []
      (rows onePage fourPage)
    ]
  where
  rows onePage fourPage =
    zipWith mkrow (filter isBlk $ secContent onePage)
      (map secContent $ filter (not . isBlk) $ secContent fourPage)
  mkrow :: Element -> [Element]  -> [TableCell]
  mkrow  p@(Blk para) cont = [flattenElement p,concatMap flattenElement cont]
  mkrow _ _                = []

