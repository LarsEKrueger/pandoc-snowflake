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
module Snowflake.Tab.Overview
( tabOverview
) where

import           Data.Maybe
import           Data.Text
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
  onePara <- findSection db ["design", "one-paragraph-summary"]
  return $
    (
      buildTable2
      [[titledSection 3 premise "Premise", titledSection 3 intConf "Internal Conflict"]
      ,[titledSection 3 twist "Twist",     titledSection 3 extConf "External Conflict"]
      ]
    : buildTable2
      [[titledSection 3 onePara "One Paragraph Summary"],[]]
    : makeMenu db "charBase" ( mapMaybe contentCharBase $ secContent charBase)
    )
    ++ titledSection 3 onePage "One Page Summary"

contentCharBase :: Element -> Maybe (Text,MenuContent)
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
      ( buildTable2 
          [[titledSection 4 role "Role"
           ,titledSection 4 oneSentence "One-sentence Summary"]
          ,[titledSection 4 goal "Goal"
           ,titledSection 4 motivation "Motivation"]
          ,[titledSection 4 intConf "Internal Conflict"
           ,titledSection 4 epiphany "Epiphany"]
          ]
      : titledSection 4 oneParagraph "One-paragraph Summary"
      )
