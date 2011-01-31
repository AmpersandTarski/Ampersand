{-# OPTIONS_GHC -Wall  #-}  
module DatabaseDesign.Ampersand_Prototype.PlugPHP 
                 (
                  PhpValue(..)
                 ,PhpType(..)
                 ,PhpArgs
                 ,PhpReturn(..)
                 ,PhpAction(..)
                 ,ActionType(..)
                 ,PlugPHP(..)
                 ,makePhpPlug --make a phpplug from an ObjectDef (user-defined php plug)
                 )
where
import DatabaseDesign.Ampersand.ADL1 ( Concept(..)
           , Relation(..), Identified(..)
           , ObjectDef(..),mapExpression,mapMorphism
           )
import DatabaseDesign.Ampersand.Fspec.FPA (FPA(..),FPAble(..))
import DatabaseDesign.Ampersand_Prototype.CodeAuxiliaries (Named(..))
import DatabaseDesign.Ampersand_Prototype.CodeVariables (CodeVar(..),CodeVarIndexed(..))
import DatabaseDesign.Ampersand_Prototype.CodeStatement (PHPconcept(..))
import Data.Maybe (listToMaybe)
import Data.Plug
import DatabaseDesign.Ampersand.Fspec
import Data.List
----------------------------------------------
--PlugPHP
----------------------------------------------
data PlugPHP
 = PlugPHP { phpname   :: String       -- ^ the name of the function
           , phpfile   :: Maybe String -- ^ the file in which the plug is located (Nothing means it is built in already)
           , phpinArgs :: [CodeVar]    -- ^ the input of this plug (list of arguments)
           , phpOut    :: CodeVar      -- ^ the output of this plug. When the input does not exist, the function should return false instead of an object of this type
           , phpSafe   :: Bool         -- ^ whether the input of this plug is verified. False means that the function can be called with non-existant input, such that it does not return false as output or causes undesired side effects
           , phpfpa    :: FPA          -- ^ functie punten analyse
           }
             deriving (Show)

instance Identified PlugPHP where
  name p = phpname p
  rename p x = p{phpname=x}

instance FPAble PlugPHP where
  fpa p = phpfpa p

instance Eq PlugPHP where
  x==y = name x==name y && phpfile x == phpfile y && phpinArgs x == phpinArgs y

instance Plugable PlugPHP where
  makePlug p = case p of
                 InternalPlug _ -> error ("!Fatal (module PlugPHP 50): A plugPHP cannot be an Internal plug")
                 ExternalPlug obj -> makePhpPlug obj

data PhpValue = PhpNull | PhpObject {objectdf::ObjectDef,phptype::PhpType} deriving (Show)
data PhpType = PhpString | PhpInt | PhpFloat | PhpArray deriving (Show)
type PhpArgs = [(Int,PhpValue)]
data PhpReturn = PhpReturn {retval::PhpValue} deriving (Show)
--DO you need on::[Relation Concept]? makeFspec sets an empty list
data PhpAction = PhpAction {action::ActionType, on::[Relation Concept]} deriving (Show)
data ActionType = Create | Read | Update | Delete deriving (Show)

instance Identified PhpValue where
   name p = case p of {PhpNull -> "0"; PhpObject{objectdf=x} -> objnm x}

instance ShowHS PlugPHP where
 showHSname plug = haskellIdentifier ("plug_"++name plug)
 showHS flags indent plug
    = (intercalate indent 
         ["let x = x in -- TODO: This code should be fixed. " -- ++ intercalate (indent++"    ")
                 --         [showHSname f++indent++"     = "++showHS flags (indent++"       ") f| f<-fields p] ++indent++"in"
         ,"PlugPhp{ phpname   = " ++ (show.haskellIdentifier.name) plug
         ,"       , phpfile   = "++show (phpfile plug)
         ,"       , phpinArgs = [ "++intercalate (indent++"                   , ") [show cv| cv <-phpinArgs plug] ++ "]"
         ,"       , phpOut    = "++show (phpOut plug)
         ,"       , phpSafe   = "++show (phpSafe plug)
         ,"       , phpfpa    = " ++ showHS flags "" (fpa plug)
         ,"       }"
         ])
instance ShowHS PhpValue where
 showHSname _ = error ("!Fatal (module PlugPHP 80): PhpValue is anonymous with respect to showHS flags.")
 showHS flags _ phpVal
   = case phpVal of
        PhpNull{}   -> "PhpNull"
        PhpObject{} -> "PhpObject{ objectdf = " ++ showHSname (objectdf phpVal) ++ ", phptype  = " ++ showHS flags "" (phptype phpVal) ++ "}"

instance ShowHS PhpType where
 showHSname _ = error ("!Fatal (module PlugPHP 87): PhpType is anonymous with respect to showHS flags.")
 showHS _ indent PhpString = indent++"PhpString"
 showHS _ indent PhpInt    = indent++"PhpInt"
 showHS _ indent PhpFloat  = indent++"PhpFloat"
 showHS _ indent PhpArray  = indent++"PhpArray"

instance ShowHS PhpReturn where
 showHSname _ = error ("!Fatal (module PlugPHP 94): PhpReturn is anonymous with respect to showHS flags.")
 showHS flags indent ret = indent++"PhpReturn {retval = "++showHS flags indent (retval ret)++"}"

instance ShowHS PhpAction where
 showHSname _ = error ("!Fatal (module PlugPHP 98): PhpAction is anonymous with respect to showHS flags.")
 showHS flags indent act
   = (intercalate (indent ++"    ") 
       [ "PhpAction { action = " ++ showHS flags "" (action act)
       , "          , on     = " ++ "["++intercalate ", " (map (showHS flags "") (on act))++"]"
       , "          }"
       ])
instance ShowHS ActionType where
 showHSname _ = error ("!Fatal (module ShowHS 198): \"ActionType\" is anonymous with respect to showHS flags.")
 showHS _ indent Create = indent++"Create"
 showHS _ indent Read   = indent++"Read"
 showHS _ indent Update = indent++"Update"
 showHS _ indent Delete = indent++"Delete"

--instance XML PlugPHP where
--  mkTag p = Tag "PlugPhp" [ nameToAttr p]
--  mkXmlTree p
--   = Elem (mkTag p)
--          [ ] -- TODO
   
-----------------------------------------
--makePhpPlug
-----------------------------------------
-- | makePhpPlug is used to make user defined plugs, with PHP functions to get the data from. Note that these plug's cannot be used to store anything.
makePhpPlug :: ObjectDef -> PlugPHP
makePhpPlug obj
 = PlugPHP (name obj)    -- plname (function name)
           inFile        -- the file in which the plug is located (Nothing means BuiltIn)
           inAttrs       -- the input of this plug (list of arguments)
           outObj        -- the output of this plug (single object or scalar). If inAttrs does not exist, plug should return false.
           verifiesInput -- whether the input of this plug is verified
           (ILGV Eenvoudig) -- the number of function points to be counted for this plug
  where
   inFile :: Maybe String
   inFile = listToMaybe [ str    --objstrs obj = [["FILE=date.plug.php"]]
                        | x<-objstrs obj, 'F':'I':'L':'E':'=':str<-x ]
   inAttrs :: [CodeVar]
   inAttrs = [toAttr attr | attr<-objats obj, (or$ map (elem "PHPARG") (objstrs attr))]
   toAttr :: ObjectDef -> CodeVar
   toAttr a = CodeVar{cvIndexed=IndexByName -- TODO, read this from parameters
                     ,cvContent=Right [] -- TODO!! Allow complex objects..
                     ,cvExpression=mapExpression (mapMorphism PHPC) (objctx a)}
   outObj :: CodeVar
   outObj = CodeVar{cvIndexed=IndexByName
                   ,cvContent=Right [Named (name attr)$ toAttr attr | attr<-objats obj, notElem ["PHPARGS"] (objstrs attr)]
                   ,cvExpression=mapExpression (mapMorphism PHPC) (objctx obj)}
   verifiesInput::Bool
   verifiesInput = True   

