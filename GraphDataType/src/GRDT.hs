-- see https://haskell-haddock.readthedocs.io/en/latest/markup.html for a note on how I comment things
{-|
Module      : GRDT
Description : quasi-quoters for GRDT
Copyright   : (c) Sebastiaan Joosten, 2020
License     : BSD
Maintainer  : sample@example.com -- Le: would you like to be the primary maintainer?
Stability   : experimental
Portability : POSIX
    
This module contains the quasi-quoters for GRDT.
If you are just using this module to import Template Haskell functions,
 this is all you need to import.
-} -- TODO: write full usage instructions
module GRDT (grdt,module GRDT.Common) where
import GRDT.Common
import GRDT.Generators
import GRDT.Parser
import Language.Haskell.TH.Quote

grdt :: QuasiQuoter
grdt = QuasiQuoter {
    quoteExp  = notHandled "expressions"
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = (generateGRDT =<<) . parseGRDT
  }
  where notHandled things = error $
            things ++ " are not handled by the grdt quasiquoter."
