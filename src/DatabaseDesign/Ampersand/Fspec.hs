{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec (
     module DatabaseDesign.Ampersand.Fspec.Fspec
   , module DatabaseDesign.Ampersand.Fspec.ShowHS
   , module DatabaseDesign.Ampersand.Fspec.ShowADL
   , module DatabaseDesign.Ampersand.Fspec.ShowECA
   , module DatabaseDesign.Ampersand.Fspec.ShowXMLtiny
   , module DatabaseDesign.Ampersand.Fspec.Graphics
   , module DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Fspec
   , module DatabaseDesign.Ampersand.Fspec.ToFspec.Calc
   )
where
import DatabaseDesign.Ampersand.Fspec.Fspec (Fspc(..))
import DatabaseDesign.Ampersand.Fspec.ShowHS (ShowHS(..),fSpec2Haskell,haskellIdentifier)
import DatabaseDesign.Ampersand.Fspec.ShowADL (ShowADL(..))
import DatabaseDesign.Ampersand.Fspec.ShowECA (showECA)
import DatabaseDesign.Ampersand.Fspec.ShowXMLtiny (showXML)
import DatabaseDesign.Ampersand.Fspec.Graphics (makePicture)
import DatabaseDesign.Ampersand.Fspec.ToFspec.Calc ( deriveProofs )
import DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Fspec (makeFspec)