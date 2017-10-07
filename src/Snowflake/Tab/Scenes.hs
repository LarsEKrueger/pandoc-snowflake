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
    Table [] [AlignLeft,AlignLeft] [0.5,0.5] []
      (rows fourPage scenes)
    ]
  where
  rows fourPage scenes =
    map mkrow $ zip (filter (not.isBlk) $ secContent fourPage)
      (filter (not.isBlk) $ secContent scenes)

  mkrow :: (Element,Element) -> [TableCell]
  mkrow (fp@(Sec {}),sc@(Sec {})) =
    [concatMap flattenElement $ secContent fp
    ,concatMap flattenElement $ secContent sc]
  mkrow _ = []

