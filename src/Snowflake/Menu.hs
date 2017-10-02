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
