module Snowflake.Menu
( makeMenu
) where

import Data.Maybe

import Text.Pandoc.Definition

import Snowflake.Database

makeMenu :: Database -> String -> [(String,Database->Maybe [Block])] -> [Block]
makeMenu db menuId sectionGenerators =
  Div (menuId,["menubar"],[])
    (map fst sections)
  : map snd sections
  where
  sections = map (mkSection menuId db) $ zip [0..] sectionGenerators

mkSection :: String -> Database -> (Int,(String,Database->Maybe [Block])) -> (Block,Block)
mkSection id db (ind,(headline,mkContent)) =
  ( RawBlock (Format "HTML") $ "<span onclick=\"selectMenu('" ++ id ++ "'," ++ show ind ++ ")\">" ++ headline ++ "</span>"
  , Div ( id ++ "." ++ show ind, [ if ind == 0 then "bodyshow" else "bodyhide"], []) secCont
  )
  where
  secCont = fromMaybe [Plain [Str "content generation failure"]] $ mkContent db
