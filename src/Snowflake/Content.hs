module Snowflake.Content
( titledSection
, inlinesToString
, stripSingleParagraph
) where

import           Text.Pandoc.Definition
import           Text.Pandoc.Shared

import           Snowflake.Database

titledSection :: Int -> Element -> String -> [Block]
titledSection _ (Blk _) _ = []
titledSection depth (Sec _ _ _ _ cont) headline =
  Header depth ("",[],[]) [Str headline]
  : concatMap flattenElement cont

inlinesToString :: [Inline] -> String
inlinesToString = concatMap itos

itos :: Inline -> String
itos (Str s)         = s
itos (Emph e)        = inlinesToString e
itos (Strong s)      = inlinesToString s
itos (Strikeout s)   = inlinesToString s
itos (Superscript s) = inlinesToString s
itos (Subscript s)   = inlinesToString s
itos (SmallCaps s)   = inlinesToString s
itos (Span _ s)      = inlinesToString s
itos Space           = " "
itos SoftBreak       = " "
itos LineBreak       = " "
itos _               = ""

stripSingleParagraph :: [Block] -> [Block]
stripSingleParagraph [Para x] = [Plain x]
stripSingleParagraph x        = x
