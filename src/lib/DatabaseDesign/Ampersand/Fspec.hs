{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec
   ( module DatabaseDesign.Ampersand.Fspec.Fspec
   , module DatabaseDesign.Ampersand.Fspec.Plug
   , module DatabaseDesign.Ampersand.Fspec.ShowHS
   , module DatabaseDesign.Ampersand.Fspec.ShowADL
   , module DatabaseDesign.Ampersand.Fspec.ShowECA
   , module DatabaseDesign.Ampersand.Fspec.ShowMeatGrinder
   , module DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram
   , module DatabaseDesign.Ampersand.Fspec.Graphic.Graphics
   , module DatabaseDesign.Ampersand.Fspec.ToFspec.Calc
   , module DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Fspec
   , module DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms
   , module DatabaseDesign.Ampersand.Fspec.FPA
   , module DatabaseDesign.Ampersand.Fspec.Motivations
   )
where
import DatabaseDesign.Ampersand.Fspec.Fspec
       (Fspc(..), concDefs, FProcess(..), ECArule(..), plugFields, lookupCpt, metaValues)
import DatabaseDesign.Ampersand.Fspec.Plug
       (PlugInfo(..), PlugSQL(..), SqlField(..), SqlFieldUsage(..), SqlType(..), tblcontents,
        requiredFields, requires, plugpath, Plugable(..),
        showSQL, fldauto, isPlugIndex)
import DatabaseDesign.Ampersand.Fspec.ShowHS
       (ShowHS(..), ShowHSName(..), fSpec2Haskell, haskellIdentifier)
import DatabaseDesign.Ampersand.Fspec.ShowADL (ShowADL(..), LanguageDependent(..))
import DatabaseDesign.Ampersand.Fspec.ShowECA (showECA)
import DatabaseDesign.Ampersand.Fspec.ShowMeatGrinder (meatGrinder)
import DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram
       (clAnalysis, cdAnalysis, ClassDiag(..))
import DatabaseDesign.Ampersand.Fspec.Graphic.Graphics
       (makePicture,writePicture,Picture(..), PictureReq(..),imagePath)
import DatabaseDesign.Ampersand.Fspec.ToFspec.Calc
       (deriveProofs,showProof,showPrf)
import DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Fspec
       (makeFspec)
import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms
       (conjNF, disjNF, cfProof, simplify)
import DatabaseDesign.Ampersand.Fspec.FPA
       ( fPoints)
import DatabaseDesign.Ampersand.Fspec.Motivations
       (Meaning(..),Motivated(..))
       