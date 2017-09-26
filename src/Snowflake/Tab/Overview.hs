module Snowflake.Tab.Overview
( tabOverview
) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared

import Snowflake.Database
import Snowflake.Menu
import Snowflake.Content

tabOverview :: [Element] -> Maybe [Block]
tabOverview db = do
  premise <- findSection db ["design","premise"]
  intConf <- findSection db ["design","internal-conflict"]
  extConf <- findSection db ["design","external-conflict"]
  twist <- findSection db ["design","twist"]
  onePage <- findSection db ["design","one-page-summary"]
  charBase <- findSection db ["design","characters"]
  return $
    Table [] [AlignLeft,AlignLeft] [0.0,0.0] []
      [[titledSection premise "Premise", titledSection intConf "Internal Conflict"]
      ,[titledSection twist "Twist",     titledSection extConf "External Conflict"]
      ]
    -- : makeMenu db "charBase" $ map extractCharBase $ sectionContent charBase
    : titledSection onePage "One Page Summary"

-- extractCharBase :: Element -> ([Inline],

