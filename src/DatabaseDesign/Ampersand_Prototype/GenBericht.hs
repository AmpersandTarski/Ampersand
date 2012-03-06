{-# LANGUAGE NamedFieldPuns #-}  
module DatabaseDesign.Ampersand_Prototype.GenBericht where

import Prelude hiding (writeFile)
import Data.List
import Text.CSV
import System.FilePath
import System.Directory
import Control.Monad
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
-- TODO: only show Rel and Flp Rel? give error otherwise?
--       what about Typ, Brk etc.?

fatal :: Int -> String -> a
fatal = fatalMsg "GenBericht"

doGenBericht :: Fspc -> Options -> IO ()
doGenBericht fSpec opts =
 do { verboseLn opts "Generating 'Berichtendefinities'..."
    ; createDirectoryIfMissing True $ combine (dirPrototype opts) "Berichten"
    ; let entities = genEntity_Interfaces $ interfaceS fSpec
    ; let berichtenCSV = allEntitiesToCSV entities
    ; when (development opts) $ verboseLn opts $ layout berichtenCSV
    ; genFile "Berichten/Berichten.csv" $ printSemicolonSeparated berichtenCSV
    ; genFile "Berichten/Gegevenswoordenboek.html" $ genGegevensWB entities
    ; genFile "Berichten/Berichtdefinitie.html" $ genBerichtDef entities
    }
 where genFile filename contents = 
        do { writeFile (combine (dirPrototype opts) filename) contents
           ; verboseLn opts $ "\nGenerated file "++filename
           }

-- an intermediate data type, so we can easily generate to several output formats
data Entity = Entity { entName       :: String
                     , depth         :: Int
                     , cardinality   :: String
                     , definition    :: String
                     , refType :: String
                     , properties    :: [Entity]
                     } deriving Show
 

genEntity_Interfaces :: [Interface] -> [Entity]
genEntity_Interfaces interfaces = map (genEntity_Interface interfaces) interfaces

genEntity_Interface :: [Interface] -> Interface -> Entity
genEntity_Interface interfaces interface = genEntity_ObjDef interfaces 0 (ifcObj interface)

genEntity_ObjDef :: [Interface] -> Int -> ObjectDef -> Entity
genEntity_ObjDef interfaces dpth objDef = 
    Entity { entName = name objDef
           , depth = dpth
           , cardinality = card $ objctx objDef
           , definition  = def $ objctx objDef 
           , refType     = name (target $ objctx objDef)
           , properties  =
               case objmsub objDef of
                 Nothing -> []
                 Just (Box objs)        -> map (genEntity_ObjDef interfaces (dpth+1)) objs           
                 Just (InterfaceRef nm) -> map (genEntity_ObjDef interfaces (dpth+1)) $ objsForInterfaceNamed nm
           }
 where card e = (if isTot e then "1" else "0")++".."++(if isUni e then "1" else "*")
       
       -- take the DEFINE SRC or DEFINE TGT definition for the target concept if it exists,
       -- otherwise take the definition of the concept itself
       def rel = if relTargetDef /= "" 
                 then relTargetDef 
                 else case cptdf $ target rel of
                          Cd {cddef=def} : _ | def /= "" -> def
                          _                              -> "** NO DEFINITION **"
        where relTargetDef = case rel of -- target def of relation, or source def if relation is flipped
                               (ERel (Rel{reldcl=Sgn{decConceptDef=Just (RelConceptDef Tgt def)}}))        -> def
                               (EFlp (ERel (Rel{reldcl=Sgn{decConceptDef=Just (RelConceptDef Src def)}}))) -> def
                               _                                              -> ""       
       
       showMeaning meaning = concat [ aMarkup2String m | m@A_Markup{amLang=Dutch} <- ameaMrk meaning ]

       objsForInterfaceNamed :: String -> [ObjectDef]
       objsForInterfaceNamed nm =
         case objmsub $ ifcObj $ getInterfaceByName interfaces nm of
           Just (Box objs) -> objs
           _               -> fatal 81 "Bericht interfaces have wrong format"
       -- NOTE: We ignore the interface relation for interfaces refs

allEntitiesToCSV :: [Entity] -> CSV
allEntitiesToCSV entities = ["Naam", "Card.", "Definitie", "Type"] : 
                            intercalate [["","","",""]] (map entityToCSV  entities)

entityToCSV :: Entity -> CSV 
entityToCSV (Entity nm dpth card def refTp props) =
  [ concat (replicate dpth ". ") ++ nm, card, def, refTp] : concatMap entityToCSV props

indentHead i lines = [(concat (replicate i ". ")++c1):line | (c1:line) <- lines]

-- Utils

getInterfaceByName :: [Interface] -> String -> Interface
getInterfaceByName interfaces nm = case [ ifc | ifc <- interfaces, name ifc == nm ] of
                                [ifc] -> ifc
                                _     -> fatal 63 $ "getInterface by name: multiple or no interfaces named "++show nm 
layout :: [[String]] -> String
layout lines = 
  let columns = transpose lines
      widths = map (sum . map length) columns
      formatColumn col = let width = maximum . map length $ col
                         in  map (fill width) col
  in  unlines . map unwords . transpose . map formatColumn $ columns
 where fill i str = str ++ take (i - length str) (replicate i ' ') 
 
-- Modified version of Text.CSV.printCSV
printSemicolonSeparated :: CSV -> String
printSemicolonSeparated records = unlines (printRecord `map` records)
    where printRecord = intercalate ";" . map printField
          printField f = "\"" ++ concatMap escape f ++ "\""
          escape '"' = "\"\""
          escape x = [x]


 
-- Html

genGegevensWB entities = gegevensWB_Header ++ 
                         gegevensWb_Toc entities ++
                         gegevensWB_Middle ++
                         concatMap (gegevensWB_Element entities) entities ++
                         gegevensWB_Footer

gegevensWb_Toc entities = unlines
  [ "      <li>" ++ mkLocalLink concept concept ++ "</li>"
  | Entity{ entName = concept, definition = def } <- entities
  ]


-- TODO: it's not the concept, but the interface name, yet refTp is a concept? or also an interface name?
gegevensWB_Element entities (Entity concept _ _ def refTp props) =
  wbElement_Header concept ++
  concatMap (wbElement_Element entities concept) props ++ 
  wbElement_Footer 

wbElement_Header concept  =
  "    " ++ mkAnchor concept ++
  "    <div class=\"gegevenselement\">\n" ++
  "      <div class=\"objectclass\">"++concept++"</div>\n" ++
  "      <div class=\"definition\">"++concept++"</div>\n" ++
  "      <table class=\"properties\">\n" ++
  "        <tr><td class=\"head\">Property term</td><td class=\"head\">Cardinality</td><td class=\"head\">Representation term</td></tr>\n" 
  
wbElement_Element entities parentConcept (Entity concept _ card def refTp props) =
  "        <tr class=\"property-abie-"++escapeNonAlphaNum parentConcept++"\">\n" ++
  "          <td class=\"property_term\" title=\""++def++"\">"++def++"</td>\n" ++
  -- NOTE: don't want def twice here
  "          <div class=\"info d1e110\"></div><td class=\"cardinality\">"++card++"</td>\n" ++
  "          <td class=\"representationterm\">" ++ mkLink entities refTp refTp ++ "</a></td>\n" ++
  -- TODO: leave out <a> if this is not a defined data type 
  "        </tr>\n"
  

wbElement_Footer =
  "      </table>\n" ++
  "    </div>\n" 

mkAnchor entityName = "<a name=\"abie-"++escapeNonAlphaNum entityName++"\" id=\"abie-"++escapeNonAlphaNum entityName++"\"> </a>\n"

mkLocalLink name html = "<a href=\"#abie-"++escapeNonAlphaNum name++"\" title=\""++name++"\">" ++
                        html ++ "</a>"

mkLink entities name html =
  if isEntity entities name 
  then "<a id=\"abie-"++escapeNonAlphaNum name++"\" href=\"Gegevenswoordenboek.html#abie-"++escapeNonAlphaNum name++"\">\n"++
       html ++ "</a>"
  else html

isEntity entities name = length (filter ((==name) . entName) entities) > 0 

genBerichtDef entities =
  berichtDef_Header ++
  berichtDef_Toc entities ++
  berichtDef_Middle ++
  concatMap (berichtDef_ElementLine entities) entities ++
  berichtDef_Footer

berichtDef_Toc entities = unlines
  [ "    <li><a href=\"#abie-"++escapeNonAlphaNum entNm++"\" title=\"\">"++entNm++"</a></li>"
  | Entity{ entName = entNm, definition = def } <- entities
  ]
  
  
berichtDef_ElementLine entities (Entity entNm depth card def refTp props) =
  "        <tr>\n" ++
  "          <td class=\"bold\" title=\""++def++"\""++padding++">\n" ++
  "            " ++ mkLink entities refTp entNm ++
  "          </td>\n" ++
  "          <td>"++card++"</td>\n" ++
  "          <td>"++def++"</td>\n" ++
  "        </tr>\n" ++
  concatMap (berichtDef_ElementLine entities) props
 where padding = if depth > 0  then " style=\"padding-left:"++show (depth*25)++"px\"" else ""
  
  
-- Html-page strings
  
berichtDef_Header :: String
berichtDef_Header =   
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n" ++
  "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" ++
  "<head>\n" ++
  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />\n" ++
  "<title>Documentation for Aanlevering Zaakstuk</title>\n" ++
  "<link rel=\"stylesheet\" type=\"text/css\" href=\"lib/css/style.css\" media=\"screen\" />\n" ++
  "<link rel=\"stylesheet\" type=\"text/css\" href=\"lib/css/style-print.css\" media=\"print\" />\n" ++
  "</head>\n" ++
  "<body>\n" ++
  "<h2>Business Document - AanleveringZaakstuk_BD-023011</h2>\n" ++
  "<div id=\"main\">\n" ++
  "  <h3>Document Information</h3>\n" ++
  "  <table class=\"bdDescription\">\n" ++
  "    <tr><td class=\"descriptionHeader\">Object Class Term</td><td>Aanlevering Zaakstuk</td></tr>\n" ++
  "    <tr><td class=\"descriptionHeader\">Qualifier Term</td><td></td></tr>\n" ++
  "    <tr><td class=\"descriptionHeader\">Version</td><td>1.1.0.0</td></tr>\n" ++
  "    <tr><td class=\"descriptionHeader\">Unique Identifier</td><td>BD-023011</td></tr>\n" ++
  "    <tr><td class=\"descriptionHeader\">Release Identifier</td><td>1.1RC02</td></tr>\n" ++
  "    <tr><td class=\"descriptionHeader\">Date</td><td>2011-11-25</td></tr>\n" ++
  "    <tr><td class=\"descriptionHeader\">Definition</td><td>Gebruikt om stukken aan te leveren aan de Rechtsprekende Instantie.</td></tr>\n" ++
  "    <tr><td class=\"descriptionHeader\">Comments</td><td>'Generiek' BD Aanlevering Zaakstuk.&#13;1.1 heeft voorschot salaris toegevoegd - specifiek voor DDI</td></tr>\n" ++
  "    <tr><td class=\"descriptionHeader\">Document Header</td><td>The XML schema for this business document includes the <a href=\"lib/headerdoc/ebvheader.html\">http://data.justid.nl/common/header-1</a> header(s).</td></tr>\n" ++
  "  </table>\n" ++
  "  <h3>Document Properties</h3>\n" ++
  "  <ol>\n"
  
berichtDef_Middle =
  "  </ol>\n" ++
  "  <div id=\"main-abie\">\n" ++
  "    <div class=\"gegevenselement\">\n" ++
  "      <table class=\"properties\">\n" ++
  "        <tr><td class=\"head\">Property term</td><td class=\"head\">Cardinality</td><td class=\"head\">Definition</td></tr>\n"

berichtDef_Footer =
  "  </div>\n" ++
  "</div>\n" ++
  "</body>\n" ++
  "</html>\n"
  
gegevensWB_Header =
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n" ++
  "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" ++
  "<head>\n" ++
  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />\n" ++
  "<title>Rechtspraak 2011-11-25 20:14:20</title>\n" ++
  "<link rel=\"stylesheet\" type=\"text/css\" href=\"lib/css/style.css\" media=\"screen\" />\n" ++
  "<link rel=\"stylesheet\" type=\"text/css\" href=\"lib/css/style-print.css\" media=\"print\" />\n" ++
  "</head>\n" ++
  "<body>\n" ++
  "<div id=\"index\">\n" ++
  "  <div id=\"abie\" class=\"index-group\">\n" ++
  "    <h2>Aggregate BIEs</h2>\n" ++
  "    <ol>\n" 
           
gegevensWB_Middle =
  "    </ol>\n" ++
  "  </div>\n" ++
  "</div>\n" ++
  "<div id=\"main\">\n" ++
  "  <div id=\"main-abie\">\n"
  
gegevensWB_Footer =  
  "  </div>\n" ++
  "</div>\n" ++
  "</body>\n" ++
  "</html>\n"
