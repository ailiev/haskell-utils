--
-- Parsec stuff
-- 

module Parsec (
               maybeParse,
               withOptionalPrefix
              )
where


import Text.ParserCombinators.Parsec as P (try, (<|>), option)
import SashoLib

-- parse rule p and place the result in a Maybe (Nothing if it failed)
maybeParse p = P.option Nothing (P.try p >>== Just)

-- parse a grammar like:
-- A -> BC | C
-- in the case where B may be a prefix of C (so the obvious shortcut of "maybe B >> C" fails, we
-- need to backtrack out of the C)
p `withOptionalPrefix` pre = (P.try   (do pre_val <- pre
                                          p_val   <- p
                                          return (Just pre_val, p_val)))
                             <|>
                             (do p_val <- p
                                 return (Nothing, p_val))
