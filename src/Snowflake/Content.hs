module Snowflake.Content
( titledSection
) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared

import Snowflake.Database

titledSection :: Element -> String -> [Block]
titledSection (Blk _) _ = []
titledSection (Sec _ _ _ _ cont) headline =
  Header 3 ("",[],[]) [Str headline]
  : concatMap flattenElement cont

