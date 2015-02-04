module Database.Design.Ampersand.Prototype.GenFrontend (doGenFrontend) where

import Prelude hiding (putStrLn,readFile)
import Data.Maybe
import System.FilePath
import Text.StringTemplate
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.Core.AbstractSyntaxTree
import Database.Design.Ampersand.FSpec.FSpec
import Database.Design.Ampersand.Prototype.ProtoUtil

fatal :: Int -> String -> a
fatal = fatalMsg "GenFrontend"

doGenFrontend :: FSpec -> IO ()
doGenFrontend fSpec =
 do { putStrLn "Generating new frontend.." 
    ; let feInterfaces = buildInterfaces fSpec
    ; traverseInterfaces fSpec feInterfaces
    ; return ()
    }

-- TODO: interface ref: Editable relations from interface def of ref, or origin?
--       both may result in unexpected editability. Maybe take intersection?

data FEInterface = FEInterface String FEObject deriving Show

data FEObject = FEObject String FESubinterface deriving Show

-- subinterface/subobject?
data FESubinterface = FEBox (Maybe String) [FEObject] | FEAtoms deriving Show

buildInterfaces :: FSpec -> [FEInterface]
buildInterfaces fSpec = map (buildInterface fSpec) $ interfaceS fSpec

buildInterface :: FSpec -> Interface -> FEInterface
buildInterface fSpec interface = FEInterface (name interface) $ buildObject fSpec {- (ifcParams interface) -} (ifcObj interface)
    

buildObject :: FSpec -> ObjectDef -> FEObject
buildObject fSpec object = FEObject (name object) $ buildSubinterface fSpec (objmsub object)

buildSubinterface :: FSpec -> Maybe SubInterface -> FESubinterface
buildSubinterface fSpec mSubIfc =
  case mSubIfc of
    Just (InterfaceRef nm) -> case filter (\ifc -> name ifc == nm) $ interfaceS fSpec of -- Follow interface refs
                                [i] -> buildSubinterface fSpec (objmsub $ ifcObj i) -- TODO: skip interface root object, what about exp and editable rels?
                                []  -> fatal 44 $ "Referenced interface " ++ nm ++ " missing"
                                _   -> fatal 45 $ "Multiple declarations of referenced interface " ++ nm
    Just (Box _ cl objects) -> FEBox cl $ map (buildObject fSpec) objects
    Nothing                 -> FEAtoms



traverseInterfaces :: FSpec -> [FEInterface] -> IO ()
traverseInterfaces fSpec ifcs =
 do { putStrLn $ show $ map name (interfaceS fSpec)
    ; mapM_ (traverseInterface fSpec) $ ifcs
    }

traverseInterface :: FSpec -> FEInterface -> IO ()
traverseInterface fSpec (FEInterface interfaceName obj) =
 do { verboseLn (getOpts fSpec) $ "\nTop-level interface: " ++ interfaceName
    ; lns <- traverseObject fSpec 0 Nothing obj
    ; template <- readTemplate fSpec "views/TopLevelInterface.html"
    ; let contents = render $ setAttribute "ampersandVersionStr" ampersandVersionStr
                            $ setAttribute "interfaceName"       interfaceName
                            $ setAttribute "contents"            (unlines lns)
                            $ template

    ; let filename = interfaceName ++ ".html" -- TODO: escape
    ; writePrototypeFile fSpec ("views" </> filename) $ contents 
    }

traverseObject :: FSpec -> Int -> Maybe String -> FEObject -> IO [String]
traverseObject fSpec depth mParentClass (FEObject nm subifc) =
 do { verboseLn (getOpts fSpec) $ replicate depth ' ' ++ nm

    ; lns <- traverseSubinterface fSpec depth subifc

    ; let clss = fromMaybe "BOX" mParentClass
    ; template <- readTemplate fSpec $ "views/Box-" ++ clss ++ "-child.html"
    ; let contents = render $ setAttribute "contents" (unlines $ indent 2 lns)
                            $ template
    
    ; return $ lines contents
    }

traverseSubinterface :: FSpec -> Int -> FESubinterface -> IO [String]
traverseSubinterface fSpec depth subIntf =
  case subIntf of
    FEAtoms ->
     do { template <- readTemplate fSpec $ "views/Atoms-DEFAULT-parent.html"
        ; let contents = render $ template

        ; return $ lines contents
        }
    FEBox mCl objects ->
     do { verboseLn (getOpts fSpec) $ replicate depth ' ' ++ "BOX" ++ maybe "" (\c -> "<"++c++">") mCl
        ; let clss = fromMaybe "BOX" mCl

        ; lnss <- mapM (traverseObject fSpec (depth + 1) mCl) objects

        ; template <- readTemplate fSpec $ "views/Box-" ++ clss ++ "-parent.html"
        ; let contents = render $ setAttribute "class"    clss
                                $ setAttribute "contents" (unlines $ indent 2 $ concat lnss)
                                $ template

        ; return $ lines contents
        } {-
  where wrapChild :: StringTemplate String -> [String] -> [String]
        wrapChild childTemplate childLns = lines $
          render $ setAttribute "contents" (unlines $ indent 2 childLns)
                 $ childTemplate
  -}

readTemplate :: FSpec -> String -> IO (StringTemplate String)
readTemplate fSpec templatePath =
 do { res <- readUTF8File $ (getTemplateDir fSpec) </> templatePath
    ; case res of
        Left err          -> error $ "Cannot read template file " ++ templatePath ++ "\n" ++ err
        Right templateStr -> return $ newSTMP templateStr
    }
