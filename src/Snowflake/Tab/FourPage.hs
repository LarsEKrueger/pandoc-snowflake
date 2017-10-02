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
    Table [] [AlignLeft,AlignLeft] [0.0,0.0] []
      (rows onePage fourPage)
    ]
  where
  rows onePage fourPage =
    map mkrow $ zip (filter isBlk $ secContent onePage)
      (map secContent $ filter (not . isBlk) $ secContent fourPage)
  mkrow :: (Element,[Element]) -> [TableCell]
  mkrow (p@(Blk para),cont) = [flattenElement p,concatMap flattenElement cont]
  mkrow _                   = []
