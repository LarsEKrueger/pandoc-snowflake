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

module Snowflake.Tab.Details
( tabCharDet
) where

import           Data.Maybe
import           Text.Pandoc.Definition
import           Text.Pandoc.Shared

import           Snowflake.Content
import           Snowflake.Database
import           Snowflake.Menu

tabCharDet :: [Element] -> MenuContent
tabCharDet db = do
  charBase <- findSection db ["design","character-details"]
  return $
    makeMenu db "charDet" $ mapMaybe contentCharDet $ secContent charBase

contentCharDet :: Element -> Maybe (String,MenuContent)
contentCharDet (Blk _) = Nothing
contentCharDet char =
  if headline == "Character"
     then Nothing
     else Just (headline,Just [content])
  where
  headline = inlinesToString $ secHeadline char
  content = Div ("",["charDetTab"],[]) [Table [] [AlignLeft,AlignLeft] [0.0, 0.0] [] rows]
  rows = filter emptyRow $ map sec2row $ secContent char

emptyRow :: [[Block]] -> Bool
emptyRow []     = False
emptyRow [[],_] = False
emptyRow [_,[]] = False
emptyRow [_,_]  = True
emptyRow _      = False

sec2row :: Element -> [[Block]]
sec2row (Blk _) = []
sec2row (Sec _ _ _ headline cont) =
  [[Plain headline],
   stripSingleParagraph $ concatMap flattenElement cont]
