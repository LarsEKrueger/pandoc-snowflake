module Snowflake.Tab.Synopsis
( tabCharSyn
) where

import           Data.Maybe
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

contentCharSyn :: Element -> Maybe (String,MenuContent)
contentCharSyn (Blk _) = Nothing
contentCharSyn char =
  if headline == "Character"
     then Nothing
     else Just (headline,content)
  where
  headline = inlinesToString $ secHeadline char
  content = Just $ concatMap flattenElement $ secContent char


