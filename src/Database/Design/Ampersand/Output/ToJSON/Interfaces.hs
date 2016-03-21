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
  , ifcJSONboxClass           :: Maybe String
  , ifcJSONifcObject          :: JSONObjectDef
  } deriving (Generic, Show)
data JSONObjectDef = JSONObjectDef
  { ifcJSONid                 :: String
  , ifcJSONlabel              :: String
  , ifcJSONviewId             :: Maybe String
  , ifcJSONNormalizationSteps :: [String] -- Not used in frontend. Just informative for analisys
  , ifcJSONrelation           :: Maybe String
  , ifcJSONrelationIsFlipped  :: Maybe Bool
  , ifcJSONcrud               :: JSONcrud
  , ifcJSONexpr               :: JSONexpr
  , ifcJSONsubinterfaces      :: Maybe JSONSubInterface
  } deriving (Generic, Show)
data JSONSubInterface = JSONSubInterface
  { subJSONboxClass           :: Maybe String
  , subJSONifcObjects         :: Maybe [JSONObjectDef]
  , subJSONrefSubInterfaceId  :: Maybe String
  , subJSONrefIsLinTo         :: Maybe Bool
  , subJSONcrudC              :: Bool
  , subJSONcrudR              :: Bool
  , subJSONcrudU              :: Bool
  , subJSONcrudD              :: Bool
  } deriving (Generic, Show)
data JSONcrud = JSONcrud
  { crudJSONread              :: Bool
  , crudJSONcreate            :: Bool
  , crudJSONupdate            :: Bool
  , crudJSONdelete            :: Bool
  } deriving (Generic, Show)
data JSONexpr = JSONexpr
  { exprJSONsrcConcept        :: String
  , exprJSONtgtConcept        :: String
  , exprJSONisUni             :: Bool
  , exprJSONisTot             :: Bool
  , exprJSONisProp            :: Bool
  , exprJSONisIdent           :: Bool
  , exprJSONquery             :: String
  } deriving (Generic, Show)

instance ToJSON JSONSubInterface where
  toJSON = amp2Jason
instance ToJSON Interfaces where
  toJSON = amp2Jason
instance ToJSON JSONInterface where
  toJSON = amp2Jason
instance ToJSON JSONObjectDef where
  toJSON = amp2Jason
instance ToJSON JSONcrud where
  toJSON = amp2Jason
instance ToJSON JSONexpr where
  toJSON = amp2Jason
  
instance JSON FSpec Interfaces where
 fromAmpersand fSpec _ = Interfaces (map (fromAmpersand fSpec) (interfaceS fSpec ++ interfaceG fSpec))

instance JSON ([Declaration], SubInterface) JSONSubInterface where
 fromAmpersand fSpec (editableRels, sub) = 
   case sub of 
     Box _ cl objs         -> JSONSubInterface
       { subJSONboxClass           = cl
       , subJSONifcObjects         = Just . map (fromAmpersand fSpec) . zip (repeat editableRels) $ objs
       , subJSONrefSubInterfaceId  = Nothing
       , subJSONrefIsLinTo         = Nothing
       , subJSONcrudC              = True
       , subJSONcrudR              = True
       , subJSONcrudU              = True
       , subJSONcrudD              = True
       }
     InterfaceRef isLink nm cr -> JSONSubInterface
       { subJSONboxClass           = Nothing
       , subJSONifcObjects         = Nothing
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
  , ifcJSONboxClass           = Nothing -- todo, fill with box class of toplevel ifc box
  , ifcJSONifcObject          = fromAmpersand fSpec (ifcParams interface, ifcObj interface)
  }

instance JSON ObjectDef JSONcrud where
 fromAmpersand fSpec object = JSONcrud
  { crudJSONread              = crudR . objcrud $ object
  , crudJSONcreate            = crudC . objcrud $ object
  , crudJSONupdate            = crudU . objcrud $ object
  , crudJSONdelete            = crudU . objcrud $ object
  }
  
instance JSON ObjectDef JSONexpr where
 fromAmpersand fSpec object = JSONexpr
  { exprJSONsrcConcept        = name srcConcept
  , exprJSONtgtConcept        = name tgtConcept
  , exprJSONisUni             = isUni normalizedInterfaceExp
  , exprJSONisTot             = isTot normalizedInterfaceExp
  , exprJSONisProp            = isProp normalizedInterfaceExp
  , exprJSONisIdent           = isIdent normalizedInterfaceExp
  , exprJSONquery             = prettySQLQuery fSpec 0 normalizedInterfaceExp
  }
  where
    normalizedInterfaceExp = conjNF (getOpts fSpec) $ objctx object
    (srcConcept, tgtConcept, mEditableDecl) =
      case getExpressionRelation normalizedInterfaceExp of
        Just (src, decl, tgt, isFlipped) ->
          (src, tgt, Just (decl, isFlipped))
        Nothing -> (source normalizedInterfaceExp, target normalizedInterfaceExp, Nothing) -- fall back to typechecker type
 
instance JSON ([Declaration], ObjectDef) JSONObjectDef where
 fromAmpersand fSpec (editableRels, object) = JSONObjectDef
  { ifcJSONid                 = escapeIdentifier . name $ object
  , ifcJSONlabel              = name object
  , ifcJSONviewId             = case getDefaultViewForConcept fSpec tgtConcept of
                                 Just Vd{vdlbl=vId} -> Just vId
                                 Nothing            -> Nothing
  , ifcJSONNormalizationSteps = showPrf showADL.cfProof (getOpts fSpec).objctx $ object 
  , ifcJSONrelation           = fmap (showHSName . fst) mEditableDecl
  , ifcJSONrelationIsFlipped  = fmap               snd  mEditableDecl
  , ifcJSONcrud               = fromAmpersand fSpec object
  , ifcJSONexpr               = fromAmpersand fSpec object
  , ifcJSONsubinterfaces      = fmap (fromAmpersand fSpec . builder) (objmsub object)
  }
  where
    builder s = (editableRels,s)
    normalizedInterfaceExp = conjNF (getOpts fSpec) $ objctx object
    (srcConcept, tgtConcept, mEditableDecl) =
      case getExpressionRelation normalizedInterfaceExp of
        Just (src, decl, tgt, isFlipped) ->
          (src, tgt, Just (decl, isFlipped))
        Nothing -> (source normalizedInterfaceExp, target normalizedInterfaceExp, Nothing) -- fall back to typechecker type
    