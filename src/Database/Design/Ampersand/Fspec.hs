module Database.Design.Ampersand.Fspec
   ( module Database.Design.Ampersand.Fspec.Fspec
   , module Database.Design.Ampersand.Fspec.Plug
   , module Database.Design.Ampersand.Fspec.ShowHS
   , module Database.Design.Ampersand.Fspec.ShowADL
   , module Database.Design.Ampersand.Fspec.ShowECA
   , module Database.Design.Ampersand.Fspec.ShowMeatGrinder
   , module Database.Design.Ampersand.Fspec.Graphic.ClassDiagram
   , module Database.Design.Ampersand.Fspec.Graphic.Graphics
   , module Database.Design.Ampersand.Fspec.ToFspec.Calc
   , module Database.Design.Ampersand.Fspec.ToFspec.ADL2Fspec
   , module Database.Design.Ampersand.Fspec.ToFspec.NormalForms
   , module Database.Design.Ampersand.Fspec.Motivations
   )
where
import Database.Design.Ampersand.Fspec.Fspec
       (Fspc(..), concDefs, FProcess(..), ECArule(..), plugFields, lookupCpt, metaValues)
import Database.Design.Ampersand.Fspec.Plug
       (PlugInfo(..), PlugSQL(..), SqlField(..), SqlFieldUsage(..), SqlType(..), tblcontents,
        requiredFields, requires, plugpath, Plugable(..),
        showSQL, fldauto, isPlugIndex)
import Database.Design.Ampersand.Fspec.ShowHS
       (ShowHS(..), ShowHSName(..), fSpec2Haskell, haskellIdentifier)
import Database.Design.Ampersand.Fspec.ShowADL (ShowADL(..), LanguageDependent(..))
import Database.Design.Ampersand.Fspec.ShowECA (showECA)
import Database.Design.Ampersand.Fspec.ShowMeatGrinder (meatGrinder)
import Database.Design.Ampersand.Fspec.Graphic.ClassDiagram
       (clAnalysis, cdAnalysis, ClassDiag(..))
import Database.Design.Ampersand.Fspec.Graphic.Graphics
       (makePicture,writePicture,Picture(..), PictureReq(..),imagePath)
import Database.Design.Ampersand.Fspec.ToFspec.Calc
       (deriveProofs,showProof,showPrf, commaEngPandoc, commaNLPandoc, commaEngPandoc', commaNLPandoc')
import Database.Design.Ampersand.Fspec.ToFspec.ADL2Fspec
       (makeFspec)
import Database.Design.Ampersand.Fspec.ToFspec.NormalForms
       (conjNF, disjNF, cfProof, simplify)
import Database.Design.Ampersand.Fspec.Motivations
       (Meaning(..),Motivated(..))
       