{-# LANGUAGE DeriveDataTypeable #-}
module Database.Design.Ampersand.Prototype.GenFrontend (doGenFrontend) where

import Prelude hiding (putStrLn,readFile)
import Data.Data
import Data.List
import System.Directory
import System.FilePath
import Text.StringTemplate
import Text.StringTemplate.GenericStandard () -- only import instances
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.FSpec.ToFSpec.NormalForms
import qualified Database.Design.Ampersand.Misc.Options as Opts
import Database.Design.Ampersand.Prototype.ProtoUtil

fatal :: Int -> String -> a
fatal = fatalMsg "GenFrontend"

{- TODO
- Get rid of -parent/child templates?
- HStringTemplate hangs on uninitialized vars in anonymous template? (maybe only fields?)
- Factorize common parts for FEObject
- isRoot is a bit dodgy (maybe make dependency on ONE and SESSIONS a bit more apparent)
- Escape everything

- Keeping templates as statics requires that the static files are written before templates are used.
  Maybe we should keep them as cabal data-files instead. (file extensions and directory structure are predictable)
-}

getTemplateDir :: FSpec -> String
getTemplateDir fSpec = Opts.dirPrototype (getOpts fSpec) </> 
                         "templates"
--                         "debugTemplates"

-- For useful info on the template language, see
-- https://theantlrguy.atlassian.net/wiki/display/ST4/StringTemplate+cheat+sheet
                         
doGenFrontend :: FSpec -> IO ()
doGenFrontend fSpec =
 do { putStrLn "Generating new frontend.." 
    ; let feInterfaces = buildInterfaces fSpec
    ; traverseInterfaces fSpec feInterfaces
    }


data FEInterface = FEInterface { _ifcName :: String, _ifcMClass :: Maybe String -- _ disables 'not used' warning for fields
                               , _ifcExp :: Expression, _ifcSource :: A_Concept, _ifcTarget :: A_Concept
                               , _ifcRoles :: [String], _ifcEditableRels :: [Expression], _ifcObj :: FEObject }

data FEObject = FEBox    { objName :: String, _objMClass :: Maybe String
                         , objExp :: Expression, objSource :: A_Concept, objTarget :: A_Concept
                         , _objIsEditable :: Bool, _objNavInterfaces :: [NavInterface] -- though unused, NavInterfaces could also make sense for Box 
                         , _ifcSubIfcs :: [FEObject] }
              | FEAtomic { objName :: String
                         , objExp :: Expression, objSource :: A_Concept, objTarget :: A_Concept
                         , _objIsEditable :: Bool, _objNavInterfaces :: [NavInterface] } deriving Show

data NavInterface = NavInterface { _navIfcName :: String, _navIfcRoles :: [String] } deriving Show

buildInterfaces :: FSpec -> [FEInterface]
buildInterfaces fSpec = map (buildInterface fSpec allIfcs) allIfcs
  where
    allIfcs :: [Interface]
    allIfcs = interfaceS fSpec
    
buildInterface :: FSpec -> [Interface] -> Interface -> FEInterface
buildInterface fSpec allIfcs ifc =
  let editableRels = ifcParams ifc
      obj = buildObject editableRels (ifcObj ifc)
  in  FEInterface  (objName obj) (ifcClass ifc) (objExp obj) (objSource obj) (objTarget obj) (ifcRoles ifc) editableRels obj
  -- NOTE: due to Amperand's interface data structure, name, expression, source, and target are taken from the root object 
  where    
    buildObject :: [Expression] -> ObjectDef -> FEObject
    buildObject editableRels object =
      let iExp = conjNF (getOpts fSpec) $ objctx object
          (isEditable, src, tgt) = 
            case getExpressionRelation iExp of
              Nothing                          -> (False, source iExp, target iExp)
              Just (declSrc, decl, declTgt, _) -> (EDcD decl `elem` editableRels, declSrc, declTgt) 
                                               -- if the expression is a relation, use the (possibly narrowed type) from getExpressionRelation
          navIfcs = [ NavInterface (name nIfc) nRoles -- only consider interfaces that share roles with the one we're building 
                    | nIfc <- allIfcs
                    ,  (source . objctx . ifcObj $ nIfc) == tgt
                    , let nRoles =  ifcRoles nIfc `intersect` ifcRoles ifc
                    , not . null $ nRoles
                    ]
       in  case objmsub object of
            Nothing                  -> FEAtomic (name object) iExp src tgt isEditable navIfcs
            Just (InterfaceRef nm)   -> case filter (\rIfc -> name rIfc == nm) $ allIfcs of -- Follow interface ref
                                          []      -> fatal 44 $ "Referenced interface " ++ nm ++ " missing"
                                          (_:_:_) -> fatal 45 $ "Multiple declarations of referenced interface " ++ nm
                                          [i]     -> let editableRels' = editableRels `intersect` ifcParams i
                                                     in  buildObject editableRels' (ifcObj i)
            Just (Box _ mCl objects) ->FEBox (name object) mCl iExp src tgt isEditable navIfcs $
                                              map (buildObject editableRels) objects
    
  
traverseInterfaces :: FSpec -> [FEInterface] -> IO ()
traverseInterfaces fSpec ifcs =
 do { verboseLn (getOpts fSpec) $ show $ map name (interfaceS fSpec)
    ; mapM_ (traverseInterface fSpec) $ ifcs
    }

traverseInterface :: FSpec -> FEInterface -> IO ()
traverseInterface fSpec (FEInterface interfaceName _ iExp iSrc iTgt roles editableRels obj) =
 do { verboseLn (getOpts fSpec) $ "\nTop-level interface: " ++ show interfaceName ++ " [" ++ name iSrc ++ "*"++ name iTgt ++ "] "
    ; lns <- traverseObject fSpec 0 obj
    ; template <- readTemplate fSpec "views/TopLevelInterface.html"
    ; let contents = renderTemplate template $
                       setAttribute "isRoot"                   (name (source iExp) `elem` ["ONE", "SESSION"]) .
                       setAttribute "roles"                    [ show r | r <- roles ] . -- show string, since StringTemplate does not elegantly allow to quote and separate
                       setAttribute "editableRelations"        [ show $ name r | EDcD r <- editableRels] . -- show name, since StringTemplate does not elegantly allow to quote and separate
                       setManyAttrib [ ("ampersandVersionStr", ampersandVersionStr)
                                     , ("interfaceName",       interfaceName)
                                     , ("source",              name iSrc) -- TODO: escape
                                     , ("target",              name iTgt) -- TODO: escape
                                     , ("contents",            intercalate "\n" . indent 4 $ lns) -- intercalate, because unlines introduces a trailing \n
                                     ]

    ; let filename = interfaceName ++ ".html" -- TODO: escape
    ; writePrototypeFile fSpec ("app/views" </> filename) $ contents 
    }

-- Helper data structure to pass attribute values to HStringTemplate
data SubObjectAttr = SubObjAttr { subObjName :: String, isBLOB ::Bool
                                , subObjContents :: String } deriving (Show, Data, Typeable)
 
traverseObject :: FSpec -> Int -> FEObject -> IO [String]
traverseObject fSpec depth obj =
  case obj of
    FEAtomic nm _ src tgt isEditable navInterfaces ->
     do { verboseLn (getOpts fSpec) $ replicate depth ' ' ++ "ATOMIC "++show nm ++ 
                                        " [" ++ name src ++ "*"++ name tgt ++ "], " ++
                                        (if isEditable then "" else "not ") ++ "editable"
        
        -- For now, we choose specific template based on target concept. This will probably be too weak. 
        -- (we might want a single concept to could have multiple presentations, e.g. BOOL as checkbox or as string)
        ; let specificTemplatePth = "views/Atomic-" ++ name tgt ++ ".html" -- TODO: escape
        ; hasSpecificTemplate <- doesTemplateExist fSpec $ specificTemplatePth
        ; template <- if hasSpecificTemplate 
                      then readTemplate fSpec $ specificTemplatePth
                      else readTemplate fSpec $ "views/Atomic.html" -- default template
                
        ; verboseLn (getOpts fSpec) $ unlines [ replicate depth ' ' ++ "-NAV: "++ show n ++ " for "++ show rs 
                                              | NavInterface n rs <- navInterfaces ]
        ; let mNavInterface = case navInterfaces of -- TODO: do something with roles here. For now, simply use the first interface, if any.
                                []                        -> Nothing
                                NavInterface ifcName _ :_ -> Just ifcName
                                                                              
        ; return $ lines $ renderTemplate template $ 
                             setAttribute "isEditable" isEditable .
                             setAttribute "navInterface" mNavInterface . -- TODO: escape
                             setManyAttrib [ ("name",   nm)       -- TODO: escape
                                           , ("source", name src) -- TODO: escape
                                           , ("target", name tgt) -- TODO: escape
                                           ]        
        }
    FEBox nm mClass _ src tgt isEditable _ subObjs ->
     do { verboseLn (getOpts fSpec) $ replicate depth ' ' ++ "BOX" ++ maybe "" (\c -> "<"++c++">") mClass ++
                                        " " ++ show nm ++ " [" ++ name src ++ "*"++ name tgt ++ "], " ++
                                        (if isEditable then "" else "not ") ++ "editable"

        ; subObjAttrs <- mapM traverseSubObject subObjs
                
        ; let clssStr = maybe "" (\cl -> "-" ++ cl) mClass
        ; parentTemplate <- readTemplate fSpec $ "views/Box" ++ clssStr ++ ".html"
        
        ; return $ lines $ renderTemplate parentTemplate $ 
                             setAttribute "isEditable" isEditable .
                             setAttribute "subObjects" subObjAttrs .
                             setManyAttrib [ ("name",     nm) -- TODO: escape
                                           , ("source",   name src) -- TODO: escape
                                           , ("target",   name tgt) -- TODO: escape
                                           ]
        }
  where traverseSubObject subObj = 
         do { lns <- traverseObject fSpec (depth + 1) subObj
            ; return SubObjAttr{ subObjName = objName subObj -- TODO: escape
                               , isBLOB = name (target $ objExp subObj) == "BLOB"
                               , subObjContents = intercalate "\n" $ indent 8 lns
                               -- Indentation is not context sensitive, so some templates will
                               -- be indented a bit too much (we take the maximum necessary value now)
                               } 
            }
      
-- data type to keep template and source file together for better errors
data Template = Template (StringTemplate String) String

-- TODO: better abstraction for specific template and fallback to default
doesTemplateExist :: FSpec -> String -> IO Bool
doesTemplateExist fSpec templatePath =
 do { let absPath = getTemplateDir fSpec </> templatePath
    ; doesFileExist absPath
    }
 
readTemplate :: FSpec -> String -> IO Template
readTemplate fSpec templatePath =
 do { let absPath = getTemplateDir fSpec </> templatePath
    ; res <- readUTF8File $ absPath
    ; case res of
        Left err          -> error $ "Cannot read template file " ++ templatePath ++ "\n" ++ err
        Right templateStr -> return $ Template (newSTMP templateStr) absPath
    }

-- having Bool attributes prevents us from using a [(String, String)] parameter for attribute settings
renderTemplate :: Template -> (StringTemplate String -> StringTemplate String) -> String
renderTemplate (Template template absPath) setAttrs =
  let appliedTemplate = setAttrs template
  in  case checkTemplateDeep appliedTemplate of
             ([],  [],    []) -> render appliedTemplate
             (parseErrs@(_:_), _, _)        -> templateError $ concat [ "Parse error in " ++ tmplt ++ " " ++ err ++ "\n" 
                                                                      | (tmplt,err) <- parseErrs]
             ([], attrs@(_:_), _)        -> templateError $ "Uninitialized template attributes: " ++ show attrs
             ([], [], ts@(_:_)) -> templateError $ "Missing invoked templates: " ++ show ts -- should not happen as we don't invoke templates
  where templateError msg = error $ "\n\n*** TEMPLATE ERROR in:\n" ++ absPath ++ "\n\n" ++ msg
