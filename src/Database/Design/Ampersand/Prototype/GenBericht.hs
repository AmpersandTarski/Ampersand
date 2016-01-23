{-# LANGUAGE NamedFieldPuns #-}
module Database.Design.Ampersand.Prototype.GenBericht (doGenBericht) where

import Prelude hiding (writeFile)
import Data.List
import Text.CSV
import System.FilePath
import System.Directory
import Control.Monad
import Database.Design.Ampersand
import Database.Design.Ampersand.Basics (fatal)
-- TODO: only show Rel and Flp Rel? give error otherwise?
--       what about Typ, Brk etc.?

-- an intermediate data type, so we can easily generate to several output formats
data Entity = Entity { entName ::      String
                     , depth ::        Int
                     , cardinality ::  String
                     , definition ::   String
                     , refType ::      String
                     , associations :: [Entity]
                     } deriving Show

doGenBericht :: FSpec -> IO ()
doGenBericht fSpec =
 do { verboseLn (getOpts fSpec) "Generating 'Berichtendefinities'..."
    ; createDirectoryIfMissing True $ combine (dirPrototype (getOpts fSpec)) "Berichten"
    ; let entities = genEntity_Interfaces $ interfaceS fSpec
    ; let berichtenCSV = allEntitiesToCSV entities
    ; when (development (getOpts fSpec)) $ verboseLn (getOpts fSpec) $ layout berichtenCSV
    ; genFile "Berichten/Berichten.csv" $ printSemicolonSeparated berichtenCSV
    ; genFile "Berichten/Gegevenswoordenboek.html" $ genGegevensWB entities
    ; genFile "Berichten/Berichtdefinitie.html" $ genBerichtDef entities
    }
 where
   genFile filename contents =
        do { writeFile (combine (dirPrototype (getOpts fSpec)) filename) contents
           ; verboseLn (getOpts fSpec) $ "\nGenerated file "++filename
           }
   genEntity_Interfaces :: [Interface] -> [Entity]
   genEntity_Interfaces interfaces' = map genEntity_Interface interfaces'
     where
       genEntity_Interface :: Interface -> Entity
       genEntity_Interface interface = genEntity_ObjDef 0 (ifcObj interface)
         where
           genEntity_ObjDef :: Int -> ObjectDef -> Entity
           genEntity_ObjDef dpth objDef =
               Entity { entName = name objDef
                      , depth = dpth
                      , cardinality  = card $ objctx objDef
                      , definition   = def $ objctx objDef
                      , refType      = name (target $ objctx objDef)
                      , associations =
                          case objmsub objDef of
                            Nothing -> []
                            Just (Box _ _ objs)    -> map (genEntity_ObjDef (dpth+1)) objs
                            Just (InterfaceRef _ nm _) -> map (genEntity_ObjDef (dpth+1)) $ objsForInterfaceNamed nm
                      }
            where card e = (if isTot e then "1" else "0")++".."++(if isUni e then "1" else "*")

                  def rel = case concDefs fSpec (target rel) of
                                Cd {cddef=def'} : _ | def' /= "" -> def'
                                _                                -> "** NO DEFINITION **"

                  objsForInterfaceNamed :: String -> [ObjectDef]
                  objsForInterfaceNamed nm =
                    case objmsub $ ifcObj $ getInterfaceByName interfaces' nm of
                      Just (Box _ _ objs) -> objs
                      _                   -> fatal 81 "Bericht interfaces have wrong format"
                  -- NOTE: We ignore the interface relation for interfaces refs

allEntitiesToCSV :: [Entity] -> CSV
allEntitiesToCSV entities = ["Naam", "Card.", "Definitie", "Type"] :
                            intercalate [["","","",""]] (map entityToCSV  entities)

entityToCSV :: Entity -> CSV
entityToCSV (Entity nm dpth card def refTp props) =
  [ concat (replicate dpth ". ") ++ nm, card, def, refTp] : concatMap entityToCSV props

-- Utils

layout :: [[String]] -> String
layout linez =
  let columns = transpose linez
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

genGegevensWB :: [Entity] -> String
genGegevensWB entities = gegevensWB_Header ++
                         gegevensWB_Toc ++
                         gegevensWB_Middle ++
                         concatMap gegevensWB_Element entities ++
                         gegevensWB_Footer
 where
  gegevensWB_Toc :: String
  gegevensWB_Toc = unlines
    [ "      <li>" ++ mkLocalLink concept' concept' ++ "</li>"
    | Entity{ entName = concept' } <- entities
    ]

  -- TODO: it's not the concept, but the interface name, yet refTp is a concept? or also an interface name?
  gegevensWB_Element :: Entity -> String
  gegevensWB_Element (Entity concept' _ _ _ _ props) =
    wbElement_Header ++
    concatMap (wbElement_Element concept') props ++
    wbElement_Footer
   where
      wbElement_Header :: String
      wbElement_Header  =
        "    " ++ mkAnchor concept' ++
        "    <div class=\"gegevenselement\">\n" ++
        "      <div class=\"objectclass\">"++concept'++"</div>\n" ++
        "      <div class=\"definition\">"++concept'++"</div>\n" ++
        "      <table class=\"properties\">\n" ++
        "        <tr><td class=\"head\">Property term</td><td class=\"head\">Cardinality</td><td class=\"head\">Representation term</td></tr>\n"
       where
        mkAnchor :: String -> String
        mkAnchor entityName = "<a name=\"abie-"++escapeNonAlphaNum entityName++"\" id=\"abie-"++escapeNonAlphaNum entityName++"\"> </a>\n"

      wbElement_Element :: String -> Entity -> String
      wbElement_Element parentConcept (Entity _ _ card def refTp _) =
        "        <tr class=\"property-abie-"++escapeNonAlphaNum parentConcept++"\">\n" ++
        "          <td class=\"property_term\" title=\""++def++"\">"++def++"</td>\n" ++
        -- NOTE: don't want def twice here
        "          <div class=\"info d1e110\"></div><td class=\"cardinality\">"++card++"</td>\n" ++
        "          <td class=\"representationterm\">" ++ mkLink entities refTp refTp ++ "</a></td>\n" ++
        -- TODO: leave out <a> if this is not a defined data type
        "        </tr>\n"

  wbElement_Footer :: String
  wbElement_Footer =
    "      </table>\n" ++
    "    </div>\n"

  gegevensWB_Header :: String
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

  gegevensWB_Middle :: String
  gegevensWB_Middle =
    "    </ol>\n" ++
    "  </div>\n" ++
    "</div>\n" ++
    "<div id=\"main\">\n" ++
    "  <div id=\"main-abie\">\n"

  gegevensWB_Footer :: String
  gegevensWB_Footer =
    "  </div>\n" ++
    "</div>\n" ++
    "</body>\n" ++
    "</html>\n"

mkLocalLink :: String -> String -> String
mkLocalLink nm html = "<a href=\"#abie-"++escapeNonAlphaNum nm++"\" title=\""++nm++"\">" ++
                        html ++ "</a>"

mkLink :: [Entity] -> String -> String -> String
mkLink entities nm html =
  if isEntity entities nm
  then "<a id=\"abie-"++escapeNonAlphaNum nm++"\" href=\"Gegevenswoordenboek.html#abie-"++escapeNonAlphaNum nm++"\">\n"++
       html ++ "</a>"
  else html

isEntity :: [Entity] -> String -> Bool
isEntity entities nm = (not.null) (filter ((==nm) . entName) entities)

genBerichtDef :: [Entity] -> String
genBerichtDef entities =
  berichtDef_Header ++
  berichtDef_Toc ++
  berichtDef_Middle ++
  concatMap berichtDef_ElementLine entities ++
  berichtDef_Footer
 where
  berichtDef_Toc :: String
  berichtDef_Toc = unlines
    [ "    <li><a href=\"#abie-"++escapeNonAlphaNum entNm++"\" title=\"\">"++entNm++"</a></li>"
    | Entity{ entName = entNm } <- entities
    ]

  berichtDef_ElementLine :: Entity -> String
  berichtDef_ElementLine (Entity entNm depth card def refTp props) =
    "        <tr>\n" ++
    "          <td class=\"bold\" title=\""++def++"\""++padding++">\n" ++
    "            " ++ mkLink entities refTp entNm ++
    "          </td>\n" ++
    "          <td>"++card++"</td>\n" ++
    "          <td>"++def++"</td>\n" ++
    "        </tr>\n" ++
    concatMap berichtDef_ElementLine props
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

  berichtDef_Middle :: String
  berichtDef_Middle =
    "  </ol>\n" ++
    "  <div id=\"main-abie\">\n" ++
    "    <div class=\"gegevenselement\">\n" ++
    "      <table class=\"properties\">\n" ++
    "        <tr><td class=\"head\">Property term</td><td class=\"head\">Cardinality</td><td class=\"head\">Definition</td></tr>\n"

  berichtDef_Footer :: String
  berichtDef_Footer =
    "  </div>\n" ++
    "</div>\n" ++
    "</body>\n" ++
    "</html>\n"

