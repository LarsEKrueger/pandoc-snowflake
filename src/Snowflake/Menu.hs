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

module Snowflake.Menu
( makeMenu
, MenuContent
) where

import           Data.Maybe

import           Text.Pandoc.Definition

import           Snowflake.Database

type MenuContent = Maybe [Block]

makeMenu :: Database -> String -> [(String,MenuContent)] -> [Block]
makeMenu db menuId sectionGenerators =
  Div (menuId,["menubar"],[])
    (map fst sections)
  : map snd sections
  where
  sections = map (mkSection menuId db) $ zip [0..] sectionGenerators

mkSection :: String -> Database -> (Int,(String,MenuContent)) -> (Block,Block)
mkSection id db (ind,(headline,content)) =
  ( RawBlock (Format "HTML") menuItemMarkup
  , Div ( divId, [ divClass ], []) secCont
  )
  where
  indStr = show ind
  divId = id ++ "." ++ indStr
  divClass = if ind == 0 then "bodyshow" else "bodyhide"
  spanClass = if ind == 0 then "select" else ""
  menuItemMarkup =
    "<span class=\""
    ++ spanClass
    ++ "\" onclick=\"selectMenu('"
    ++ id
    ++ "',"
    ++ indStr
    ++ ")\">"
    ++ headline
    ++ "</span>"
  secCont = fromMaybe [Plain [Str "content generation failure"]] content
