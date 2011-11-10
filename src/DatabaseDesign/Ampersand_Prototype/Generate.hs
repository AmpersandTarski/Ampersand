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
    
    ; verboseLn opts "Generated tables\n"
    ; verboseLn opts $ unlines $ concatMap showPlug $ [ plug | InternalPlug plug <- plugInfos fSpec]
    }
  where
    writePrototypeFile fname content =
     do { verboseLn opts ("  Generating "++fname)
        --; verboseLn opts $ content
        ; writeFile (combine (dirPrototype opts) fname) content
        }

showPlug plug =  ["Table: '"++sqlname plug++"'"] ++ 
                    (indent 4 $ blockParenthesize "[" "]" "," $ map showField $ sqlFields plug)
 where sqlFields (TblSQL  {fields = flds}) = flds
       sqlFields BinSQL  { columns = (fld1,fld2)} = [fld1, fld2]
       sqlFields ScalarSQL { column = fld} = [fld]

showField fld = ["{" ++ (if fldnull fld then "+" else "-") ++ "NUL," ++ (if flduniq fld then "+" else "-") ++ "UNQ} " ++ 
                 "'"++fldname fld ++ "':"++show (target $ fldexpr fld)]
 
generateInterfaces fSpec opts = genPhp "Generate.hs" "Interfaces.php" $
  [ "$dbName = "++showPhpStr (dbName opts)++";"
  , ""
  , "$relationTables ="
  , "  array" ] ++
       (addToLastLine ";" $ indent 4 $ blockParenthesize "(" ")" ","
         [ [showPhpStr rnm++" => array (srcConcept => "++(showPhpStr $ name $ source rel)++", tgtConcept => "++(showPhpStr $ name $ target rel)++", table => "++showPhpStr table++", srcCol => "++showPhpStr srcCol++", tgtCol => "++showPhpStr tgtCol++")"] 
           | rel@(Rel {relnm = rnm}) <- mors fSpec
           , (table,srcCol,tgtCol) <- nub $ sqlRelPlugNames fSpec (ERel rel)]) ++
  [ ""     -- the nub is because sqlRelPlugNames may yield multiple results
  , "$idRelationTables ="
  , "  array" ] ++
       (addToLastLine ";" $ indent 4 $ blockParenthesize "(" ")" "," -- todo: why do we get a list of tables and src/tgtCols??
         [ [(showPhpStr $ name c)++" => array (table => "++showPhpStr table++", srcCol => "++showPhpStr srcCol++", tgtCol => "++showPhpStr tgtCol++")"] 
           | c <- concs fSpec, (table,srcCol,tgtCol) <- sqlRelPlugNames fSpec (ERel $ I c)]) ++
  [ ""
  , "$allInterfaceObjects ="
  , "  array" ] ++
       (addToLastLine ";" $ indent 4 $ blockParenthesize  "(" ")" "," $ map (generateInterface fSpec opts) allInterfaces)
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
           ERel r ->        [ "      , 'relation' => "++showPhpStr (name r) -- only support editing on user-specified relations (no expressions, and no I or V)
                            , "      , 'relationIsFlipped' => false" 
                            ]
           EFlp (ERel r) -> [ "      , 'relation' => "++showPhpStr (name r) -- and on flipped versions of those relations
                            , "      , 'relationIsFlipped' => true" 
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
  ] ++ (indent 10 $ blockParenthesize "(" ")" "," $ map (genInterfaceObjects fSpec opts $ depth + 1) $ objats object) ++
  [ "      )"
  ]
 
blockParenthesize :: String -> String -> String -> [[String]] -> [String]
blockParenthesize open close sep [] = [open ++ close]
blockParenthesize open close sep  liness = concat [ zipWith (++) (pre:repeat "  ") (lines::[String]) 
                                                  | (pre, lines) <- zip ((open++" "): repeat (sep++" ")) liness ] ++ [close]
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

-- generatorModule is the Haskell module responsible for generation, makes it easy to track the origin of the php code
genPhp generatorModule moduleName contentLines = unlines $
  [ "<?php"
  , "// module "++moduleName++" generated by "++generatorModule
  , "// "++prototypeVersionStr
  ] ++ replicate 2 "" ++ contentLines ++
  [ "?>"
  ]
  
indent n lines = [ replicate n ' ' ++ line | line <- lines ]
