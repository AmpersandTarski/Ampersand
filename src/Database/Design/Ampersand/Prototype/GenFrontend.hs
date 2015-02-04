module Database.Design.Ampersand.Prototype.GenFrontend (doGenFrontend) where

import Prelude hiding (putStrLn,readFile)
import Data.List
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

data FEInterface = FEBox    { ifcName :: String, ifcMClass :: Maybe String, ifcSubIfcs :: [FEInterface] }
                 | FEAtomic { ifcName :: String } deriving Show

buildInterfaces :: FSpec -> [FEInterface]
buildInterfaces fSpec = map (buildInterface fSpec) $ interfaceS fSpec

buildInterface :: FSpec -> Interface -> FEInterface
buildInterface fSpec interface = buildObject fSpec {- (ifcParams interface) -} (ifcObj interface)
-- TODO: handle editable rels here, and maybe interface class

buildObject :: FSpec -> ObjectDef -> FEInterface
buildObject fSpec object =
  case objmsub object of
    Just (InterfaceRef nm) -> case filter (\ifc -> name ifc == nm) $ interfaceS fSpec of -- Follow interface refs
                                [i] -> buildInterface fSpec i -- TODO: skip interface root object, what about exp and editable rels?
                                []  -> fatal 44 $ "Referenced interface " ++ nm ++ " missing"
                                _   -> fatal 45 $ "Multiple declarations of referenced interface " ++ nm
    Just (Box _ cl objects) -> FEBox    (name object) cl $ map (buildObject fSpec) objects
    Nothing                 -> FEAtomic (name object)
  
  
traverseInterfaces :: FSpec -> [FEInterface] -> IO ()
traverseInterfaces fSpec ifcs =
 do { putStrLn $ show $ map name (interfaceS fSpec)
    ; mapM_ (traverseTopLevelInterface fSpec) $ ifcs
    }

traverseTopLevelInterface :: FSpec -> FEInterface -> IO ()
traverseTopLevelInterface _     (FEAtomic _)                  =  fatal 57 $ "Unexpected atomic top-level interface"
traverseTopLevelInterface fSpec ifc@(FEBox interfaceName _ _) =
 do { verboseLn (getOpts fSpec) $ "\nTop-level interface: " ++ interfaceName
    ; lns <- traverseInterface fSpec 0 ifc
    ; template <- readTemplate fSpec "views/TopLevelInterface.html"
    ; let contents = render $ setAttribute "ampersandVersionStr" ampersandVersionStr
                            $ setAttribute "interfaceName"       interfaceName
                            $ setAttribute "contents"            (unlines lns)
                            $ template

    ; let filename = interfaceName ++ ".html" -- TODO: escape
    ; writePrototypeFile fSpec ("views" </> filename) $ contents 
    }

traverseInterface :: FSpec -> Int -> FEInterface -> IO [String]
traverseInterface fSpec depth subIntf =
  case subIntf of
    FEAtomic _ ->
     do { template <- readTemplate fSpec $ "views/Atomic.html"
        ; let contents = render $ template
        ; putStrLn $ "ATOMIC contents" ++ show contents
        ; return $ lines contents
        }
    FEBox _ mClass ifcs ->
     do { verboseLn (getOpts fSpec) $ replicate depth ' ' ++ "BOX" ++ maybe "" (\c -> "<"++c++">") mClass

        ; childLnss <- mapM (traverseInterface fSpec (depth + 1)) ifcs

        ; let clss = maybe "" (\cl -> "-" ++ cl) mClass
        ; childTemplate <- readTemplate fSpec $ "views/Box" ++ clss ++ "-child.html"
        
        ; let wrappedChildLnss = map (wrapChild childTemplate) childLnss 
        ; putStrLn $ "wrapped child contents" ++ show wrappedChildLnss
        
        ; parentTemplate <- readTemplate fSpec $ "views/Box" ++ clss ++ "-parent.html"
        ; let contents = render $ setAttribute "class"    clss
                                $ setAttribute "contents" (intercalate "\n" $ indent 2 $ concat wrappedChildLnss)
                                $ parentTemplate

        ; putStrLn $ "BOX contents" ++ show contents

        ; return $ lines contents
        }
      where wrapChild :: StringTemplate String -> [String] -> [String]
            wrapChild childTemplate childLns = trace ("childLns"++show childLns++"\nunlines indent "++show (unlines $ indent 2 childLns)) $ lines $
              render $ setAttribute "contents" (intercalate "\n" $ indent 2 childLns)
                     $ childTemplate
      
        
readTemplate :: FSpec -> String -> IO (StringTemplate String)
readTemplate fSpec templatePath =
 do { res <- readUTF8File $ (getTemplateDir fSpec) </> templatePath
    ; case res of
        Left err          -> error $ "Cannot read template file " ++ templatePath ++ "\n" ++ err
        Right templateStr -> return $ newSTMP templateStr
    }
