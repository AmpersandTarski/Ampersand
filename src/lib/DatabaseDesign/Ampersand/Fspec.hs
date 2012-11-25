{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec (module X) where
import DatabaseDesign.Ampersand.Fspec.Fspec as X 
       (Fspc(..), ECArule(..), lookupCpt, getGeneralizations, getSpecializations)
import DatabaseDesign.Ampersand.Fspec.Plug as X
       (PlugInfo(..), PlugSQL(..), SqlField(..), SqlType(..), tblcontents,
        tblfields, requiredFields, requires, plugpath, Plugable(..),
        showSQL, fldauto, iskey)
import DatabaseDesign.Ampersand.Fspec.ShowHS as X
       (ShowHS(..), ShowHSName(..), fSpec2Haskell, haskellIdentifier)
import DatabaseDesign.Ampersand.Fspec.ShowADL as X (ShowADL(..), LanguageDependent(..))
import DatabaseDesign.Ampersand.ADL1.P2A_Converters as X (disambiguate)
import DatabaseDesign.Ampersand.Fspec.ShowECA as X (showECA)
import DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram as X
       (clAnalysis, plugs2classdiagram, cdAnalysis, ClassDiag(..))
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
