module Snowflake.Tab.Overview
( tabOverview
) where

import           Data.Maybe
import           Text.Pandoc.Definition
import           Text.Pandoc.Shared

import           Snowflake.Content
import           Snowflake.Database
import           Snowflake.Menu

tabOverview :: Database -> MenuContent
tabOverview db = do
  premise <- findSection db ["design","premise"]
  intConf <- findSection db ["design","internal-conflict"]
  extConf <- findSection db ["design","external-conflict"]
  twist <- findSection db ["design","twist"]
  onePage <- findSection db ["design","one-page-summary"]
  charBase <- findSection db ["design","characters"]
  return $
    (
    Table [] [AlignLeft,AlignLeft] [0.0,0.0] []
      [[titledSection 3 premise "Premise", titledSection 3 intConf "Internal Conflict"]
      ,[titledSection 3 twist "Twist",     titledSection 3 extConf "External Conflict"]
      ]
    : makeMenu db "charBase" ( mapMaybe contentCharBase $ secContent charBase)
    )
    ++ titledSection 3 onePage "One Page Summary"

contentCharBase :: Element -> Maybe (String,MenuContent)
contentCharBase (Blk _) = Nothing
contentCharBase char =
  if headline == "Character"
     then Nothing
     else Just (headline,content)
  where
  db = secContent char
  headline = inlinesToString $ secHeadline char
  content = do
    role <- dbgFindSection db ["role"]
    oneSentence <- dbgFindSection db ["one-sentence-summary"]
    goal <- dbgFindSection db ["goal"]
    motivation <- dbgFindSection db ["motivation"]
    intConf <- dbgFindSection db ["internal-conflict"]
    epiphany <- dbgFindSection db ["epiphany"]
    oneParagraph <- dbgFindSection db ["one-paragraph-summary"]

    return
      ( Table [] [AlignLeft,AlignLeft] [0.0,0.0] []
          [[titledSection 4 role "Role"
           ,titledSection 4 oneSentence "One-sentence Summary"]
          ,[titledSection 4 goal "Goal"
           ,titledSection 4 motivation "Motivation"]
          ,[titledSection 4 intConf "Internal Conflict"
           ,titledSection 4 epiphany "Epiphany"]
          ]
      : titledSection 4 oneParagraph "One-paragraph Summary"
      )