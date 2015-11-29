{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Database.Design.Ampersand.Output.ToJSON.Interfaces 
   (Interfaces)
where
import Database.Design.Ampersand.Output.ToJSON.JSONutils 
import Database.Design.Ampersand.Core.AbstractSyntaxTree 
import Database.Design.Ampersand.FSpec.ShowHS
import Database.Design.Ampersand.Prototype.ProtoUtil
import Database.Design.Ampersand.FSpec.ToFSpec.NormalForms
import Database.Design.Ampersand.FSpec.ToFSpec.Calc
import Database.Design.Ampersand.FSpec.ShowADL
import Database.Design.Ampersand.FSpec.SQL
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Classes

data Interfaces = Interfaces [JSONInterface] deriving (Generic, Show)
data JSONInterface = JSONInterface
  { ifcJSONinterfaceRoles     :: [String]
  , ifcJSONinvConjunctIds     :: [String]
  , ifcJSONsigConjunctIds     :: [String]
  , ifcJSONeditableConcepts   :: [String]
  , ifcJSONeditableRelations  :: [String]
  , ifcJSONobject             :: JSONObjectDef
  } deriving (Generic, Show)
data JSONObjectDef = JSONObjectDef
  { ifcJSONname             :: String
  , ifcJSONid             :: String
  , ifcJSONlabel             :: String
  , ifcJSONviewId             :: Maybe String
  , ifcJSONNormalizationSteps :: [String] -- Not used in frontend. Just informative for analisys
  , ifcJSONrelation           :: Maybe String
  , ifcJSONrelationIsEditable :: Maybe Bool
  , ifcJSONrelationIsFlipped  :: Maybe Bool
  , ifcJSONsrcConcept         :: String
  , ifcJSONtgtConcept         :: String
  , ifcJSONcrudC              :: Maybe Bool
  , ifcJSONcrudR              :: Maybe Bool
  , ifcJSONcrudU              :: Maybe Bool
  , ifcJSONcrudD              :: Maybe Bool
  , ifcJSONexprIsUni          :: Bool
  , ifcJSONexprIsTot          :: Bool
  , ifcJSONexprIsProp         :: Bool
  , ifcJSONexprIsIdent        :: Bool
  , ifcJSONexpressionSQL      :: String
  , ifcJSONboxSubInterfaces   :: Maybe JSONSubInterface
  } deriving (Generic, Show)
data JSONSubInterface = JSONSubInterface
  { subJSONboxClass           :: Maybe String
  , subJSONboxSubInterfaces   :: Maybe [JSONObjectDef]
  , subJSONrefSubInterfaceId  :: Maybe String
  , subJSONrefIsLinTo         :: Maybe Bool
  , subJSONcrudC              :: Maybe Bool
  , subJSONcrudR              :: Maybe Bool
  , subJSONcrudU              :: Maybe Bool
  , subJSONcrudD              :: Maybe Bool
  } deriving (Generic, Show)
instance ToJSON JSONSubInterface where
  toJSON = amp2Jason
instance ToJSON Interfaces where
  toJSON = amp2Jason
instance ToJSON JSONInterface where
  toJSON = amp2Jason
instance ToJSON JSONObjectDef where
  toJSON = amp2Jason
instance JSON FSpec Interfaces where
 fromAmpersand fSpec _ = Interfaces (map (fromAmpersand fSpec) (interfaceS fSpec ++ interfaceG fSpec))
instance JSON ([Declaration], SubInterface) JSONSubInterface where
 fromAmpersand fSpec (editableRels, sub) = 
   case sub of 
     Box _ cl objs         -> JSONSubInterface
       { subJSONboxClass   = cl
       , subJSONboxSubInterfaces   = Just . map (fromAmpersand fSpec) . zip (repeat editableRels) $ objs
       , subJSONrefSubInterfaceId  = Nothing
       , subJSONrefIsLinTo         = Nothing
       , subJSONcrudC              = Nothing
       , subJSONcrudR              = Nothing
       , subJSONcrudU              = Nothing
       , subJSONcrudD              = Nothing
       }
     InterfaceRef isLink nm cr -> JSONSubInterface
       { subJSONboxClass           = Nothing
       , subJSONboxSubInterfaces   = Nothing
       , subJSONrefSubInterfaceId  = Just nm
       , subJSONrefIsLinTo         = Just isLink
       , subJSONcrudC              = crudC cr
       , subJSONcrudR              = crudR cr
       , subJSONcrudU              = crudU cr
       , subJSONcrudD              = crudD cr
       }
 
instance JSON Interface JSONInterface where
 fromAmpersand fSpec interface = JSONInterface
  { ifcJSONinterfaceRoles     = map name . ifcRoles $ interface
  , ifcJSONinvConjunctIds     = map rc_id $ invConjuncts
  , ifcJSONsigConjunctIds     = map rc_id $ sigConjuncts
  , ifcJSONeditableConcepts   = map name . editableConcepts fSpec $ interface
  , ifcJSONeditableRelations  = map showHSName . ifcParams $ interface
  , ifcJSONobject             = fromAmpersand fSpec (ifcParams interface, ifcObj interface)
  }
  where
   invConjuncts = [ c | c <- ifcControls interface, any (not . ruleIsInvariantUniOrInj) $ rc_orgRules c ] -- NOTE: these two
   sigConjuncts = [ c | c <- ifcControls interface, any        isSignal                 $ rc_orgRules c ] --       may overlap
instance JSON ([Declaration], ObjectDef) JSONObjectDef where
 fromAmpersand fSpec (editableRels, object) = JSONObjectDef
  { ifcJSONname               = name object
  , ifcJSONid                 = show . escapeIdentifier . name $ object
  , ifcJSONlabel              = name object
  , ifcJSONviewId             = case getDefaultViewForConcept fSpec tgtConcept of
                                 Just Vd{vdlbl=vId} -> Just vId
                                 Nothing            -> Nothing
  , ifcJSONNormalizationSteps = showPrf showADL.cfProof (getOpts fSpec).objctx $ object 
  , ifcJSONrelation           = fmap (showHSName . fst) mEditableDecl
  , ifcJSONrelationIsEditable = fmap ((flip elem) editableRels . fst) mEditableDecl
  , ifcJSONrelationIsFlipped  = fmap               snd  mEditableDecl
  , ifcJSONsrcConcept         = name srcConcept
  , ifcJSONtgtConcept         = name tgtConcept
  , ifcJSONcrudC              = crudC . objcrud $ object
  , ifcJSONcrudR              = crudR . objcrud $ object
  , ifcJSONcrudU              = crudU . objcrud $ object
  , ifcJSONcrudD              = crudD . objcrud $ object
  , ifcJSONexprIsUni          = isUni normalizedInterfaceExp
  , ifcJSONexprIsTot          = isTot normalizedInterfaceExp
  , ifcJSONexprIsProp         = isProp normalizedInterfaceExp
  , ifcJSONexprIsIdent        = isIdent normalizedInterfaceExp
  , ifcJSONexpressionSQL      = prettySQLQuery fSpec 0 normalizedInterfaceExp
  , ifcJSONboxSubInterfaces   = fmap (fromAmpersand fSpec . builder) (objmsub object)
  }
  where
    builder s = (editableRels,s)
    normalizedInterfaceExp = conjNF (getOpts fSpec) $ objctx object
    (srcConcept, tgtConcept, mEditableDecl) =
      case getExpressionRelation normalizedInterfaceExp of
        Just (src, decl, tgt, isFlipped) ->
          (src, tgt, Just (decl, isFlipped))
        Nothing -> (source normalizedInterfaceExp, target normalizedInterfaceExp, Nothing) -- fall back to typechecker type
    