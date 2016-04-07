{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Database.Design.Ampersand.Output.ToJSON.Interfaces 
   (Interfaces)
where
import Database.Design.Ampersand.Output.ToJSON.JSONutils 
import Database.Design.Ampersand.Core.AbstractSyntaxTree 
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
  , ifcJSONcrud               :: JSONCruds
  , ifcJSONexpr               :: JSONexpr
  , ifcJSONsubinterfaces      :: Maybe JSONSubInterface
  } deriving (Generic, Show)
data JSONSubInterface = JSONSubInterface
  { subJSONboxClass           :: Maybe String
  , subJSONifcObjects         :: Maybe [JSONObjectDef]
  , subJSONrefSubInterfaceId  :: Maybe String
  , subJSONrefIsLinTo         :: Maybe Bool
  , subJSONcrud               :: Maybe JSONCruds
  } deriving (Generic, Show)
data JSONCruds = JSONCruds
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
instance ToJSON JSONCruds where
  toJSON = amp2Jason
instance ToJSON JSONexpr where
  toJSON = amp2Jason
  
instance JSON FSpec Interfaces where
 fromAmpersand fSpec _ = Interfaces (map (fromAmpersand fSpec) (interfaceS fSpec ++ interfaceG fSpec))

instance JSON SubInterface JSONSubInterface where
 fromAmpersand fSpec sub = 
   case sub of 
     Box _ cl objs         -> JSONSubInterface
       { subJSONboxClass           = cl
       , subJSONifcObjects         = Just . map (fromAmpersand fSpec) $ objs
       , subJSONrefSubInterfaceId  = Nothing
       , subJSONrefIsLinTo         = Nothing
       , subJSONcrud               = Nothing
       }
     InterfaceRef isLink nm cr -> JSONSubInterface
       { subJSONboxClass           = Nothing
       , subJSONifcObjects         = Nothing
       , subJSONrefSubInterfaceId  = Just (escapeIdentifier nm)
       , subJSONrefIsLinTo         = Just isLink
       , subJSONcrud               = Just (fromAmpersand fSpec cr)
       }
 
instance JSON Interface JSONInterface where
 fromAmpersand fSpec interface = JSONInterface
  { ifcJSONinterfaceRoles     = map name . ifcRoles $ interface
  , ifcJSONboxClass           = Nothing -- todo, fill with box class of toplevel ifc box
  , ifcJSONifcObject          = fromAmpersand fSpec (ifcObj interface)
  }

instance JSON Cruds JSONCruds where
 fromAmpersand _ crud = JSONCruds
  { crudJSONread              = crudR crud
  , crudJSONcreate            = crudC crud
  , crudJSONupdate            = crudU crud
  , crudJSONdelete            = crudU crud
  }
  
instance JSON ObjectDef JSONexpr where
 fromAmpersand fSpec object = JSONexpr
  { exprJSONsrcConcept        = name srcConcept
  , exprJSONtgtConcept        = name tgtConcept
  , exprJSONisUni             = isUni normalizedInterfaceExp
  , exprJSONisTot             = isTot normalizedInterfaceExp
  , exprJSONisProp            = maybe False isProp mdcl
  , exprJSONisIdent           = isIdent normalizedInterfaceExp
  , exprJSONquery             = prettySQLQuery fSpec 0 normalizedInterfaceExp
  }
  where
    normalizedInterfaceExp = conjNF (getOpts fSpec) $ objctx object
    (srcConcept, mdcl,tgtConcept) =
      case getExpressionRelation normalizedInterfaceExp of
        Just (src, dcl , tgt, _) ->
          (src, Just dcl, tgt)
        Nothing -> (source normalizedInterfaceExp, Nothing, target normalizedInterfaceExp) -- fall back to typechecker type
 
instance JSON ObjectDef JSONObjectDef where
 fromAmpersand fSpec object = JSONObjectDef
  { ifcJSONid                 = escapeIdentifier . name $ object
  , ifcJSONlabel              = name object
  , ifcJSONviewId             = fmap name viewToUse
  , ifcJSONNormalizationSteps = showPrf showADL.cfProof (getOpts fSpec).objctx $ object 
  , ifcJSONrelation           = fmap (showDcl . fst) mEditableDecl
  , ifcJSONrelationIsFlipped  = fmap            snd  mEditableDecl
  , ifcJSONcrud               = fromAmpersand fSpec (objcrud object)
  , ifcJSONexpr               = fromAmpersand fSpec object
  , ifcJSONsubinterfaces      = fmap (fromAmpersand fSpec) (objmsub object)
  }
  where
    showDcl :: Declaration -> String
    showDcl dcl = name dcl ++ (show . sign) dcl
    viewToUse = case objmView object of
                 Just nm -> Just $ lookupView fSpec nm
                 Nothing -> getDefaultViewForConcept fSpec tgtConcept
    normalizedInterfaceExp = conjNF (getOpts fSpec) $ objctx object
    (tgtConcept, mEditableDecl) =
      case getExpressionRelation normalizedInterfaceExp of
        Just (_ , decl, tgt, isFlipped) ->
          (tgt, Just (decl, isFlipped))
        Nothing -> (target normalizedInterfaceExp, Nothing) -- fall back to typechecker type
    