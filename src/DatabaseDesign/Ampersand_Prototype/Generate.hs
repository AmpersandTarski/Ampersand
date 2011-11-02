module DatabaseDesign.Ampersand_Prototype.Generate (generateAll) where

import DatabaseDesign.Ampersand_Prototype.CoreImporter  
import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)
import Data.List
import Data.Maybe
import System.FilePath               
import DatabaseDesign.Ampersand_Prototype.Version 

import DatabaseDesign.Ampersand_Prototype.RelBinGenSQL

generateAll :: Fspc -> Options -> IO ()
generateAll fSpec opts =
 do { verboseLn opts "Experimental Generation"
    ; writePrototypeFile "Interfaces.php" $ generateInterfaces fSpec opts
    }
  where
    writePrototypeFile fname content =
     do { verboseLn opts ("  Generating "++fname)
        --; verboseLn opts $ content
        ; writeFile (combine (dirPrototype opts) fname) content
        }
 
  
generateInterfaces fSpec opts = genPhp "Generate.hs" "Interfaces.php" $
  phpPreliminaries ++
  ["require \"php/DatabaseUtils.php\";"
  , ""
  , "$allInterfaceObjects ="
  , "  array" ] ++
    (addToLastLine ";" $ indent 4 $ blockParenthesize $ map (generateInterface fSpec opts) allInterfaces) ++
  [ ""
  , "echo '<html>';"
  , "echo '<head>';"
  , "echo '<link href=\"css/Experimental.css\" rel=\"stylesheet\" type=\"text/css\"/>';"
  , "echo '<link href=\"http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/base/jquery-ui.css\" rel=\"stylesheet\" type=\"text/css\"/>';"
  , "echo '<script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js\"></script>';"
  , "echo '<script src=\"http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js\"></script>';"
  , "echo '<script src=\"js/Experimental.js\"></script>';"
  , "echo '<script type=\"text/javascript\">';"
  , "echo 'function init() {';"
  , "echo '  initializeLinks(getInterfacesMap());';"
  , "echo '}';"
  , "echo '';"
  , "echo generateInterfaceMap($allInterfaceObjects);"
  , "echo '</script>';"
  , "echo '</head>';"
  , "echo '<body onload=\"init()\">';"
  , "echo '<button class=\"EditButton\" onclick=\"startEditing()\">Edit</button>';"
  , "echo '<button class=\"CancelButton\" onclick=\"stopEditing(getInterfacesMap())\">Cancel</button>';"
  , ""
--  , "printBinaryTable(DB_doquer('"++dbName opts++"', getQueryOverview_as()));"
--  , "print_r( getCoDomainAtoms( 'Hello', '2', getQueryId_notIdentifies() ));"
  , ""
  , "if (isset($_REQUEST['interface']) && isset($_REQUEST['atom'])) {"
  , "    $interface=$_REQUEST['interface'];"
  , "    $atom=$_REQUEST['atom'];"
  , "    echo '<h3>Interface \\''.htmlSpecialChars($interface).'\\' for atom \\''.htmlSpecialChars($atom).'\\'</h3>';"
  , "    echo generateInterface('"++dbName opts++"', $allInterfaceObjects[$interface], $atom); "
  , "} else {"
  , "echo '<h3>Top-level interfaces</h3>';"
  , "echo topLevelInterfaceLinks($allInterfaceObjects);"
  --, " echo \"some test stuff for Hello.adl (will most likely fail for other adls)\";"
  --, " echo generateInterface('Hello', $allInterfaceObjects['Overview'], '1');"
  --, " echo generateInterface('Hello', $allInterfaceObjects['Id'], '2');"
  --, " echo generateInterface('Hello', $allInterfaceObjects['Th'], 'France');"
  , "}"
  , "echo '</body>';"
  , "echo '</html>';"
  ]     
 where allInterfaces = interfaceS fSpec ++ interfaceG fSpec

generateInterface fSpec opts interface =
  [ "// Top-level interface "++name interface ++":"
  , showPhpStr (name interface) ++" => " ] ++
  genInterfaceObjects fSpec opts 1 (ifcObj interface) 
  
-- two arrays: one for the object and one for the list of subinterfaces
genInterfaceObjects :: Fspc -> Options -> Int -> ObjectDef -> [String]
genInterfaceObjects fSpec opts depth object = indent (depth*2) $
  [ "array ( 'name' => "++showPhpStr (name object)
  , "      // relation: "++showPhpStr (show (objctx object))  -- escape for the pathological case that one of the names in the relation contains a newline
  ] ++ case objctx object of
           ERel r ->        [ "      , 'relation' => "++showPhpStr (show r) 
                            , "      , 'relationIsFlipped' => 'False'" 
                            ]
           EFlp (ERel r) -> [ "      , 'relation' => "++showPhpStr (show r) 
                            , "      , 'relationIsFlipped' => 'True'" 
                            ]          
           _             -> [ "      , 'relation' => ''" 
                            , "      , 'relationIsFlipped' => ''" 
                            ]          
  ++     
  [ "      , 'concept' => "++showPhpStr (show (target $ objctx object)) -- only needed for top level
  , "      , 'isUnivalent' => " ++ (showPhpBool $ isUni (objctx object))
  , "      , 'sqlQuery' => '" ++ (fromMaybe "" $ selectExpr fSpec 25 "src" "tgt" $ objctx object) ++ "'" -- todo give an error for Nothing                                                  
  , "      , 'subInterfaces' =>"
  , "          array"
  ] ++ (indent 10 $ blockParenthesize $ map (genInterfaceObjects fSpec opts $ depth + 1) $ objats object) ++
  [ "      )"
  ]
 
blockParenthesize :: [[String]] -> [String]
blockParenthesize [] = ["()"]
blockParenthesize liness = concat [ zipWith (++) (sep:repeat "  ") (lines::[String]) | (sep, lines) <- zip ("( ":repeat ", ") liness ] ++ [")"]
-- [["line"], ["line1", "line2", "line3"],["linea", "lineb"] ->
-- ( line
-- , line1
--   line2
--   line3
-- , linea
--   lineb
-- )

addToLastLine :: String -> [String] -> [String]
addToLastLine str [] = [str] 
addToLastLine str lines = init lines ++ [last lines ++ str] 
  
toPhp str = map replace str
 where replace ' ' = '_'
       replace c   = c
 
showPhpStr str = "'"++escapePhpStr str++"'"

-- NOTE: we assume a single quote php string, so $ and " are not escaped
escapePhpStr cs = concat [fromMaybe [c] $ lookup c [('\'', "\\'"),('\\', "\\\\"),('\n', "\\n")] | c<-cs ] -- escape '   \   \n
-- todo: escape everything else (unicode, etc)

showPhpBool b = if b then "true" else "false"

-- GenUtil
phpPreliminaries = -- Maybe this will be put in an imported Php module
  [ "error_reporting(E_ALL); "
  , "ini_set(\"display_errors\", 1);"
  ]

-- generatorModule is the Haskell module responsible for generation, makes it easy to track the origin of the php code
genPhp generatorModule moduleName contentLines = unlines $
  [ "<?php"
  , "// module "++moduleName++" generated by "++generatorModule
  , "// "++prototypeVersionStr
  ] ++ replicate 2 "" ++ contentLines ++
  [ "?>"
  ]
  
indent n lines = [ replicate n ' ' ++ line | line <- lines ]
