module Database.Design.Ampersand.FSpec
   ( module Database.Design.Ampersand.FSpec.FSpec
   , module Database.Design.Ampersand.FSpec.Plug
   , module Database.Design.Ampersand.FSpec.ShowHS
   , module Database.Design.Ampersand.FSpec.ShowADL
   , module Database.Design.Ampersand.FSpec.ShowECA
   , module Database.Design.Ampersand.FSpec.ShowMeatGrinder
   , module Database.Design.Ampersand.FSpec.ToFSpec.Calc
   , module Database.Design.Ampersand.FSpec.ToFSpec.ADL2FSpec
   , module Database.Design.Ampersand.FSpec.ToFSpec.NormalForms
   , module Database.Design.Ampersand.FSpec.Motivations
   , module Database.Design.Ampersand.FSpec.ToFSpec.CreateFspec
   )
where
import Database.Design.Ampersand.FSpec.FSpec
       (FSpec(..), concDefs, ECArule(..), plugAttributes, lookupCpt, metaValues,AAtomValue, showValADL,showValPHP,showValSQL,A_Concept,Declaration,A_Gen)
import Database.Design.Ampersand.FSpec.Plug
       (PlugInfo(..), PlugSQL(..), SqlAttribute(..), SqlAttributeUsage(..), SqlTType(..),
        Plugable(..),
        showSQL, fldauto)
import Database.Design.Ampersand.FSpec.ShowHS
       (ShowHS(..), ShowHSName(..), fSpec2Haskell, haskellIdentifier)
import Database.Design.Ampersand.FSpec.ShowADL (ShowADL(..))
import Database.Design.Ampersand.FSpec.ShowECA (showECA)
import Database.Design.Ampersand.FSpec.ShowMeatGrinder (makeMetaPopulationFile,MetaType(..))
import Database.Design.Ampersand.FSpec.ToFSpec.Calc
       (showProof,showPrf, commaEngPandoc, commaNLPandoc, commaEngPandoc', commaNLPandoc', commaPandocAnd,commaPandocOr)
import Database.Design.Ampersand.FSpec.ToFSpec.ADL2FSpec
       (makeFSpec)
import Database.Design.Ampersand.FSpec.ToFSpec.NormalForms
       (conjNF, disjNF, cfProof, simplify)
import Database.Design.Ampersand.FSpec.Motivations
       (Meaning(..),Motivated(..))
import Database.Design.Ampersand.FSpec.ToFSpec.CreateFspec 
       (createFSpec)
       