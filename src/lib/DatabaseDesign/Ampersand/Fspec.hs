{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec (module X) where
import DatabaseDesign.Ampersand.Fspec.Fspec as X 
       (Fspc(..), FProcess(..), ECArule(..), lookupCpt, metaValues, getGeneralizations, getSpecializations)
import DatabaseDesign.Ampersand.Fspec.Plug as X
       (PlugInfo(..), PlugSQL(..), SqlField(..), SqlFieldUsage(..), SqlType(..), tblcontents,
        plugFields, requiredFields, requires, plugpath, Plugable(..),
        showSQL, fldauto, isPlugIndex)
import DatabaseDesign.Ampersand.Fspec.ShowHS as X
       (ShowHS(..), ShowHSName(..), fSpec2Haskell, haskellIdentifier)
import DatabaseDesign.Ampersand.Fspec.ShowADL as X (ShowADL(..), LanguageDependent(..))
import DatabaseDesign.Ampersand.Fspec.ShowECA as X (showECA)
import DatabaseDesign.Ampersand.Fspec.ShowMeatGrinder as X (meatGrinder)
import DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram as X
       (clAnalysis, cdAnalysis, ClassDiag(..))
import DatabaseDesign.Ampersand.Fspec.Graphic.Graphics as X
       (Dotable(..), makePictureObj, printDotGraph, DrawingType(..))
import DatabaseDesign.Ampersand.Fspec.Graphic.Picture as X
       (Picture(..), PictType(..), writePicture)
import DatabaseDesign.Ampersand.Fspec.ToFspec.Calc as X
       (deriveProofs,showProof,showPrf)
import DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Fspec as X
       (makeFspec)
import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms as X
       (conjNF, disjNF, cfProof, simplify)
import DatabaseDesign.Ampersand.Fspec.FPA as X
       ( fPoints)
