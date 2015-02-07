module Database.Design.Ampersand.FSpec
   ( module Database.Design.Ampersand.FSpec.FSpec
   , module Database.Design.Ampersand.FSpec.Plug
   , module Database.Design.Ampersand.FSpec.ShowHS
   , module Database.Design.Ampersand.FSpec.ShowADL
   , module Database.Design.Ampersand.FSpec.ShowECA
   , module Database.Design.Ampersand.FSpec.ShowMeatGrinder
   , module Database.Design.Ampersand.FSpec.Graphic.ClassDiagram
   , module Database.Design.Ampersand.FSpec.Graphic.Graphics
   , module Database.Design.Ampersand.FSpec.ToFSpec.Calc
   , module Database.Design.Ampersand.FSpec.ToFSpec.ADL2FSpec
   , module Database.Design.Ampersand.FSpec.ToFSpec.NormalForms
   , module Database.Design.Ampersand.FSpec.Motivations
   , module Database.Design.Ampersand.FSpec.ToFSpec.CreateFspec
   )
where
import Database.Design.Ampersand.FSpec.FSpec
       (FSpec(..), concDefs, FProcess(..), ECArule(..), plugFields, lookupCpt, metaValues)
import Database.Design.Ampersand.FSpec.Plug
       (PlugInfo(..), PlugSQL(..), SqlField(..), SqlFieldUsage(..), SqlType(..), tblcontents,
        requiredFields, requires, plugpath, Plugable(..),
        showSQL, fldauto, isPlugIndex)
import Database.Design.Ampersand.FSpec.ShowHS
       (ShowHS(..), ShowHSName(..), fSpec2Haskell, haskellIdentifier)
import Database.Design.Ampersand.FSpec.ShowADL (ShowADL(..), LanguageDependent(..))
import Database.Design.Ampersand.FSpec.ShowECA (showECA)
import Database.Design.Ampersand.FSpec.ShowMeatGrinder (meatGrinder)
import Database.Design.Ampersand.FSpec.Graphic.ClassDiagram
       (clAnalysis, cdAnalysis, ClassDiag(..))
import Database.Design.Ampersand.FSpec.Graphic.Graphics
       (makePicture,writePicture,Picture(..), PictureReq(..),imagePath)
import Database.Design.Ampersand.FSpec.ToFSpec.Calc
       (deriveProofs,showProof,showPrf, commaEngPandoc, commaNLPandoc, commaEngPandoc', commaNLPandoc')
import Database.Design.Ampersand.FSpec.ToFSpec.ADL2FSpec
       (makeFSpec)
import Database.Design.Ampersand.FSpec.ToFSpec.NormalForms
       (conjNF, disjNF, cfProof, simplify)
import Database.Design.Ampersand.FSpec.Motivations
       (Meaning(..),Motivated(..))
import Database.Design.Ampersand.FSpec.ToFSpec.CreateFspec 
       (createFSpec,getPopulationsFrom)
       