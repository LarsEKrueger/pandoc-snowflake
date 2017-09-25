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

module Main
where

import Text.Pandoc.JSON as J
import Text.Pandoc.Walk
import Text.Pandoc.Shared
import Text.Pandoc.Definition
import qualified Data.ByteString.Lazy as B
import Data.Aeson

import Text.Show.Pretty

-- | Check if an Element is a Blk. Required for filtering blocks and sections.
isBlk :: Element -> Bool
isBlk (Blk _) = True
isBlk _ = False

-- | Stack of lists of Elements. Each item on the stack is a list of
-- semi-processed sections.
type Stack = [[Element]]

-- | Unwind the stack once, then continue to process the remaining sections.
-- This function is used when we found a Sec at a higher level (e.g. depth=2) than
-- the current top of stack item has (e.g. depth=4). This function is
-- concerned with stopping criteria only.
unwind :: Stack -> [Element] -> [Element]
unwind [] [] = []
unwind [tos] [] = reverse tos
unwind stack [] = unwind (unwind1 stack) []
unwind stack es = rebuildTree (unwind1 stack) es

-- | Unwind the stack once by appending all siblings of the top of stack as
-- children to the second item on the stack. As the list are reversed on the
-- stack, they have to be reversed before adding them as children. The
-- function is only called if the first list element (last section seen) has a
-- higher depth than the first item of the second item on the stack. This
-- function handles an orthogonal set of stopping criteria to reduce the
-- number of clauses of unwind.
unwind1 :: Stack -> Stack
unwind1 [tos , Sec d ident attr cont blkChildren : brothers] =
  [ Sec d ident attr cont (blkChildren ++ reverse tos) : brothers]
unwind1 (tos:(Sec d ident attr cont blkChildren : brothers):bos) =
  (Sec d ident attr cont (blkChildren ++ reverse tos) : brothers) : bos
unwind1 _ = undefined -- This case should not happen, but makes the compiler happy.

-- | Rebuild the tree from the flattened list of sections. It processes each
-- entry in the list of elements once. Depending on the depth of the current
-- element compared to the depth of the top of stack (ToS), we have to handle three
-- cases:
-- * The current element has a higher depth than the ToS, it is a child of the
--   ToS and will be put on the stack.
-- * The current element has the same depth as the ToS, thus it is a sibling
--   and can be prepended to the ToS list. The other siblings can't receive
--   anymore children because all higher-depth sections are children of the
--   current element.
-- * The current element has smaller depth than the ToS. In that case, we can
--   complete all open sections. We therefore unwind once. If that is not
--   enough, this check will be match again and we unwind one level etc.
-- We do have to process Blks here, because there could be some before the
-- first section.
-- We also have to handle the case that a lower-depth header follows a
-- higher-depth header at top-level of the file. In that case, unwind will
-- reduce the stack to one entry. We detect this, push out the contents of the
-- stack in reverse order and start a new stack.
rebuildTree :: Stack -> [Element] -> [Element]
rebuildTree stack [] = unwind stack []
rebuildTree stack (b@(Blk _):es) = b : rebuildTree stack es
rebuildTree [] (sec@Sec{} : es) = rebuildTree [[sec]] es

rebuildTree stack@[tpb@(tos@(Sec pd _ _ _ _):brothers)] (sec@(Sec d _ _ _ _) : es)
  | d > pd = rebuildTree ([sec]:stack) es
  | d == pd = rebuildTree [sec:tos:brothers] es
  | otherwise = reverse tpb ++ rebuildTree [[sec]] es
rebuildTree stack@((tos@(Sec pd _ _ _ _):brothers):bos) (sec@(Sec d _ _ _ _) : es)
  | d > pd = rebuildTree ([sec]:stack) es
  | d == pd = rebuildTree ((sec:tos:brothers):bos) es
  | otherwise = unwind stack (sec:es)

rebuildTree _ _ = undefined -- These cases should not happen.

main :: IO ()
main = do
  txt <- B.getContents
  let input@(Pandoc inMeta inBlocks) = (either error id $ eitherDecode' txt) :: Pandoc
      hierarchy = rebuildTree [] $ hierarchicalize inBlocks
  putStrLn $ ppShow hierarchy

  -- B.putStr $ encode $ Pandoc inMeta outBlocks
