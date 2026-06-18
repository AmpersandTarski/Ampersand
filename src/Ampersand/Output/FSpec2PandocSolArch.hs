{-# LANGUAGE ScopedTypeVariables #-}

-- | Turn an FSpec into a complete solution architecture document (Pandoc),
-- together with the data-model pictures it references.  This is the solution
-- architecture counterpart of "Ampersand.Output.FSpec2Pandoc".
module Ampersand.Output.FSpec2PandocSolArch (fSpec2PandocSolArch) where

import Ampersand.Output.FSpec2Pandoc (crossRefWrap)
import Ampersand.Output.ToPandoc
import Ampersand.Output.ToPandoc.SolutionArchitecture (chpSolutionArchitecture)
import qualified RIO.List as L
import qualified RIO.Text as T
import RIO.Time

fSpec2PandocSolArch ::
  (HasDirOutput env, HasDocumentOpts env, HasFSpecGenOpts env) =>
  env ->
  UTCTime ->
  FSpec ->
  (Pandoc, [Picture])
fSpec2PandocSolArch env now fSpec = (thePandoc, L.sortOn (name . pType) thePictures)
  where
    -- shorthand for easy localizing
    l :: LocalizedStr -> Text
    l = localize outputLang'
    outputLang' = outputLang env fSpec

    (theBlocks, chapterPictures) = chpSolutionArchitecture env fSpec

    thePandoc =
      crossRefWrap outputLang'
        . setTitle
          ( case metaValues (toText1Unsafe "title") fSpec of
              [] ->
                (text . l)
                  ( NL "Solution-architectuur van ",
                    EN "Solution architecture of "
                  )
                  <> (singleQuoted . text . fullName) fSpec
              titles -> (text . T.concat . L.nub) titles
          )
        . setAuthors
          ( case metaValues (toText1Unsafe "authors") fSpec of
              [] ->
                [ (text . l)
                    ( NL "Specificeer auteurs in Ampersand met: META \"authors\" \"<auteursnamen>\"",
                      EN "Specify authors in Ampersand with: META \"authors\" \"<author names>\""
                    )
                ]
              xs -> text <$> L.nub xs
          )
        . setDate (text (T.pack $ formatTime (lclForLang outputLang') "%-d %B %Y" now))
        . doc
        $ theBlocks

    -- Only the diagrams the solution architecture actually references: the
    -- classification structure and the logical data model of the context.
    -- (The technical data model is intentionally not part of the architecture.)
    thePictures = map (makePicture env fSpec) largePictures <> chapterPictures
      where
        largePictures =
          [ PTClassificationDiagram,
            PTLogicalDataModelOfContext False
          ]
