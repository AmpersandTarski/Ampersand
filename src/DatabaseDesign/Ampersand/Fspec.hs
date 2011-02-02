{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec (
     module DatabaseDesign.Ampersand.Fspec.Fspec
   , module DatabaseDesign.Ampersand.Fspec.ShowHS
   , module DatabaseDesign.Ampersand.Fspec.ShowADL
   , module DatabaseDesign.Ampersand.Fspec.ShowECA
   , module DatabaseDesign.Ampersand.Fspec.ShowXMLtiny
   , module DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram
   , module DatabaseDesign.Ampersand.Fspec.Graphic.Graphics
   , module DatabaseDesign.Ampersand.Fspec.Graphic.Picture
   , module DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Fspec
   , module DatabaseDesign.Ampersand.Fspec.ToFspec.Calc
   , module DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms
   , module DatabaseDesign.Ampersand.Fspec.FPA
   )
where
import DatabaseDesign.Ampersand.Fspec.Fspec (Fspc(..))
import DatabaseDesign.Ampersand.Fspec.ShowHS (ShowHS(..),fSpec2Haskell,haskellIdentifier)
import DatabaseDesign.Ampersand.Fspec.ShowADL (ShowADL(..))
import DatabaseDesign.Ampersand.Fspec.ShowECA (showECA)
import DatabaseDesign.Ampersand.Fspec.ShowXMLtiny (showXML)
import DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram (clAnalysis,cdAnalysis,ClassDiag(..))
import DatabaseDesign.Ampersand.Fspec.Graphic.Graphics (makePicture)
import DatabaseDesign.Ampersand.Fspec.Graphic.Picture (Picture(..),writePicture)
import DatabaseDesign.Ampersand.Fspec.ToFspec.Calc ( deriveProofs )
import DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Fspec (makeFspec)
import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms  (conjNF,proofPA,disjNF,simplify)
import DatabaseDesign.Ampersand.Fspec.FPA (FPA,fPoints,fpa,FPcompl(..))

