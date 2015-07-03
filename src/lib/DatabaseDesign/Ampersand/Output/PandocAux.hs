{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Output.PandocAux
      ( writepandoc
      , labeledThing
      , symDefLabel, symDefRef
      , symReqLabel, symReqRef, symReqPageRef
      , xrefSupported
      , pandocEqnArray
      , pandocEqnArrayOnelabel
      , pandocEquation
      , makeDefinition, uniquecds
      , count
      , latexEscShw, escapeNonAlphaNum
      , xrefCitation
      , texOnly_Id
      , texOnly_fun
      , texOnly_rel
      , commaEngPandoc, commaNLPandoc
      )
where

fatal :: Int -> String -> a
fatal i s = error "Output.PandocAux"
writepandoc, labeledThing
      , symDefLabel, symDefRef
      , symReqLabel, symReqRef, symReqPageRef
      , xrefSupported
      , pandocEqnArray
      , pandocEqnArrayOnelabel
      , pandocEquation
      , makeDefinition, uniquecds
      , count
      , latexEscShw, escapeNonAlphaNum
      , xrefCitation
      , texOnly_Id
      , texOnly_fun
      , texOnly_rel
      , commaEngPandoc, commaNLPandoc
      :: a
writepandoc      = fatal 42 "no pandoc implemented"
labeledThing     = fatal 42 "no pandoc implemented"
symDefLabel      = fatal 42 "no pandoc implemented"
symDefRef        = fatal 42 "no pandoc implemented"
symReqLabel      = fatal 42 "no pandoc implemented"
symReqRef        = fatal 42 "no pandoc implemented"
symReqPageRef    = fatal 42 "no pandoc implemented"
xrefSupported    = fatal 42 "no pandoc implemented"
pandocEqnArray   = fatal 42 "no pandoc implemented"
pandocEqnArrayOnelabel = fatal 42 "no pandoc implemented"
pandocEquation   = fatal 42 "no pandoc implemented"
makeDefinition   = fatal 42 "no pandoc implemented"
uniquecds        = fatal 42 "no pandoc implemented"
count            = fatal 42 "no pandoc implemented"
latexEscShw      = fatal 42 "no pandoc implemented"
escapeNonAlphaNum= fatal 42 "no pandoc implemented"
xrefCitation     = fatal 42 "no pandoc implemented"
texOnly_Id       = fatal 42 "no pandoc implemented"
texOnly_fun      = fatal 42 "no pandoc implemented"
texOnly_rel      = fatal 42 "no pandoc implemented"
commaEngPandoc   = fatal 42 "no pandoc implemented"
commaNLPandoc    = fatal 42 "no pandoc implemented"