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
