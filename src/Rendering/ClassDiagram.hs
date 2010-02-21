{-  TODO: Warningvrij maken # OPTIONS_GHC -Wall #-}
--TODO -> clean and stuff. Among which moving classdiagram2dot to Graphviz library implementation (see Classes/Graphics.hs).
--        I only helped it on its feet and I have put in the fSpec, now it generates stuff. I like stuff :)

  module Rendering.ClassDiagram (ClassDiag(..), cdAnalysis,classdiagram2dot) where
   import Char (isAlphaNum,ord,isUpper,toUpper)
   import CommonClasses (  Identified(name))
   import Collection ( Collection(empty, (>-),rd) )
   import Strings (chain) 
   import Typology (Inheritance(Isa))
   import Adl
   import Auxiliaries (eqCl)
   import Data.Plug
   import Options
   import Data.Fspec
   import Adl.ECArule

   --TODO -> copied from Auxiliaries because disabled (why disabled?)
   enc :: Bool -> String -> String
   enc upper (c:cs) | not (isAlphaNum c) = '_': htmlEnc c ++ enc upper cs
                    | isUpper c==upper   = c: enc upper cs
                    | otherwise          = '_': c: enc (not upper) cs
     where 
        htmlEnc = reverse . take 3 . (++"00") . reverse . show . ord
   enc _ "" = ""

   class CdNode a where
    nodes :: a->[String]

   

   instance CdNode ClassDiag where
    nodes (OOclassdiagram cs as rs gs _) = rd (concat (map nodes cs++map nodes as++map nodes rs++map nodes gs))

   instance CdNode Class where
    nodes (OOClass c as ms) = [c]
   instance CdNode a => CdNode [a] where
    nodes = concat.map nodes

   instance CdNode Attribute where
    nodes (OOAttr nm t fNull) = [t]

   instance CdNode Method where
    nodes m = []

   instance CdNode Data.Fspec.Association where
    nodes (OOAssoc s ml rl t mr rr) = [s,t]

   instance CdNode Aggregation where
    nodes (OOAggr d s t) = [s,t]

   instance CdNode Generalization where
    nodes (OOGener g ss) = g:ss


   cdAnalysis :: Fspc -> Options -> ClassDiag
   cdAnalysis fSpec flags = OOclassdiagram classes assocs aggrs geners (name fSpec, concs fSpec)
    where
       classes    = [ OOClass (name c) [ OOAttr a atype fNull| (a,atype,fNull)<-attrs plug] []
                    | plug <- classPlugs, fld<-fields plug, fldname fld=="i", c<-[source (fldexpr fld)]
                    , not (null (attrs plug))
                    ]
       assocs     = [ OOAssoc (nm source s) (multiplicity s) "" (nm target t) (multiplicity t) (name plug)
                    | plug <- assocPlugs
                    , if length (fields plug)==2 then True else error("!Fatal (module ClassDiagram 97): irregular association, because it has "++show ()++" fields.")
                    , [s,t]<-[fields plug]
                    ]
                    where
                     multiplicity f | fldnull f = ""
                                    | otherwise = "1..n"
                     nm f = name.f.fldexpr
                     
       aggrs      = []
       geners     = rd [ OOGener (name (fst (head gs))) (map (name.snd) gs)| Isa pcs cs<-[isa fSpec], gs<-eqCl fst pcs]
       classPlugs = [p| p<-plugs fSpec, not (null [1|fld<-fields p, flduniq fld])]
       assocPlugs = [p| p<-plugs fSpec, null [fld|fld<-fields p, flduniq fld], length (fields p)>1]
       scalarPlgs = [p| p<-plugs fSpec, null [fld|fld<-fields p, flduniq fld], length (fields p)<=1]
       attrs plug = [ (fldname fld,if null([Sym,Asy]>-multiplicities (fldexpr fld)) then "Bool" else  name (target (fldexpr fld)), fldnull fld)
                    | fld<-fields plug, fldname fld/="i"]
       attrels    = rd [d| plug<-plugs fSpec, fld<-fields plug, fldname fld/="i", d<-decls (fldexpr fld)]
       isProp d   = null([Sym,Asy]>-multiplicities d)
       scs = [d|d<-declarations fSpec] -- was: for the entire context

   shDataModel (OOclassdiagram cs as rs gs _)
    = "OOclassdiagram\n>     "++chain "\n>     "
        [ lijstopmaak (map show cs),
          lijstopmaak (map show as),
          lijstopmaak (map show rs),
          lijstopmaak (map show gs)]
    where lijstopmaak [] = "[]"
          lijstopmaak xs = "[ "++chain "\n>     , " xs++"\n>     ]"

   classdiagram2dot  cd@(OOclassdiagram cs as rs gs (_, concspat))
            = "digraph G {bgcolor=transparent\n" ++        
      --        "    fontname = \"Courier\" \n" ++
      --        "    fontsize = 18 \n" ++
      --        "    node [ \n" ++
      --        "            fontname = \"Courier\" \n" ++
      --        "            fontsize = 8 \n" ++
      --        "            shape = \"record\" \n" ++
      --        "    ] \n" ++
              "    edge [ \n" ++
      --        "            fontname = \"Courier\" \n" ++
              "            fontsize = 11"++(if layout=="neato" then ", len = 3" else "")++" \n" ++
              "    ]\n" ++
              classes2dot cs (nodes cd>-nodes cs)++ "\n" ++
              associations2dot as ++ "\n" ++
              aggregations2dot rs ++ "\n" ++
              generalizations2dot gs ++
              "\n}\n"

          where
          layout = "dot"
          classes2dot :: [Class] -> [String] -> [Char]
          classes2dot cs os
           = defaultclass ++
             (if null cs then "" else "\n" ++ chain "\n" (map class2dot cs))++
             (if null os then "" else "\n" ++ chain "\n" (map clas2dot os))
             where defaultclass = "    Node [shape = box] \n"
          clas2dot :: String -> String
          clas2dot n = spaces 5 ++ alias n ++ " [shape=box label=\""++n++"\"]"
          class2dot :: Class -> [Char]
          class2dot (OOClass n as ms) = spaces 5 ++ alias n ++ " [\n" ++
                                        spaces 7 ++ "shape=plaintext \n" ++
                                                (classlabel n as ms) ++
                                                    "\n     ]"
            where
              classlabel n as ms = spaces 7 ++ "label =<" ++
                                     (indent 10 (dottable tableopts tablecontent)) ++ "\n" ++
                                   spaces 7 ++ ">"
                    where
                      tableopts = " BGCOLOR=\"white\" BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\""
                      tablecontent =  (dotrow "" (dotcell " BGCOLOR=\"lightgray\" ALIGN=\"center\""
                                                          (dotfont " COLOR=\"red\""
                                                                   n))) ++
                                      (if null as && null ms then "" else dotrow "" (dotcell "" (attribs2dot as))) ++
                                      (if null ms then "" else dotrow "" (dotcell "" (methods2dot ms)))

              attribs2dot :: [Attribute] -> [Char]
              attribs2dot [] = emptydottable notableborderopts
              attribs2dot as = (dottable notableborderopts (chain "" (map attrib2dot as)))
 
              attrib2dot :: Attribute -> [Char]
              attrib2dot (OOAttr n t fNull) = dotrow "" (dotcell " ALIGN=\"left\"" ((if fNull then "o " else "+ ") ++ n ++ " : " ++ t))

              methods2dot :: [Method] -> [Char]
              methods2dot [] = emptydottable notableborderopts
              methods2dot ms = dottable notableborderopts (chain "" (map method2dot ms))

              method2dot :: Method -> [Char]
              method2dot m =  dotrow "" (dotcell " ALIGN=\"left\"" ("+ " ++ show m ))

  -------------------------------
  --        ASSOCIATIONS:      --
  -------------------------------
          alias nm
           = if map toUpper nm=="NODE" ||
                map toUpper nm=="EDGE"
             then (enc True . head) [ nm++show i | i<-[1..], not ((nm++show i) `elem` map name (concspat) )]
             else enc True nm
          associations2dot :: [Data.Fspec.Association] -> [Char]
          associations2dot as = chain "\n" (map association2dot as) ++ "\n"
          association2dot :: Data.Fspec.Association -> [Char]
          association2dot (OOAssoc from m1 n1 to m2 n2) =
              "      edge [ \n" ++
              "              arrowhead = \"none\" \n" ++
              "              arrowtail = \"none\" \n" ++
              (if null (nametable m2) then "" else "              headlabel = " ++ nametable m2 ++ "\n") ++
              (if null (nametable m1) then "" else "              taillabel = " ++ nametable m1 ++ "\n") ++
              "              label = \"" ++ n2 ++ "\" \n" ++
              "      ]\n" ++
              "       " ++ alias from ++ " -> " ++ alias to
              where 
                 nametable "" = "\"\""
                 nametable name = dothtml (dottable notableborderopts 
                                              (dotrow "" (dotcell "" name)))


  -------------------------------
  --        AGGREGATIONS:      --
  -------------------------------
          aggregations2dot :: [Aggregation] -> [Char]
          aggregations2dot rs = chain "\n" (map aggregation2dot rs) ++ "\n"
          aggregation2dot :: Aggregation -> [Char]
 
          aggregation2dot (OOAggr del from to) =
              "      edge [ \n" ++
              "              headlabel = \"\"\n"    ++
              "              taillabel = \"\"\n"    ++
              "              arrowtail = " ++ tail del ++" \n" ++
              "              arrowhead = \"none\" \n" ++
              "              label =\"\"" ++
              "      ]\n" ++
              "       " ++ alias from ++ " -> " ++ alias to
              where
                 tail Open  = "\"odiamond\""
                 tail Close = "\"diamond\""
 

 
  -------------------------------
  --        GENERALIZATIONS:   --       -- TODO : Wat bedoelt stef hier mee?? Wat is de syntax?
  -------------------------------
          generalizations2dot :: [Generalization] -> [Char]
          generalizations2dot gs = chain "\n" (map generalization2dot gs) ++ "\n"
 
          generalization2dot :: Generalization -> [Char]
          generalization2dot (OOGener a []) = ""
          generalization2dot (OOGener a subs) = (genEdge a firstsub) ++ (generalization2dot (OOGener a restsubs))
           where
             firstsub = head subs
             restsubs = tail subs
             genEdge a b =
              "      edge [ \n" ++
              "              headlabel = \"\"\n"    ++
              "              taillabel = \"\"\n"    ++
              "              arrowtail = \"none\" \n" ++
              "              arrowhead = onormal \n" ++
              "              color = red" ++
              "              label =\"\"" ++
              "      ]\n" ++
              "       " ++ alias a ++ " -> " ++ alias b ++ "\n"

   dothtml :: String -> String
   dothtml content = "<\n  " ++ content ++ "\n>"

   dottable :: String -> String -> String
     -- This function is designed to keep in mind that Graphviz does not cope with
     -- empty tables. In case the table has no content, the TABEL taggs are filled
     -- with a single dotrow with no contents.
   dottable opts a = "\n<TABLE" ++ opts ++">" ++ (indent 2 trya ) ++ "\n</TABLE>"
     where
       trya :: String
       trya  = if (onlyignorechars a) then (dotrow "" "") else a

   dotrow :: String -> String -> String
     -- This function is designed to keep in mind that Graphviz does not cope with
     -- empty rows. In case the row has no content, the taggs are filled
     -- with a single dotcell with no contents.
   dotrow opts a   = "\n<TR"    ++ opts ++">" ++ (indent 2 trya) ++ "\n</TR>"
     where
       trya :: String
       trya  = if (onlyignorechars a) then (dotcell "" "") else a

   dotcell :: String -> String -> String
   dotcell opts a  = "\n<TD"    ++ opts ++">" ++ (indent 2 trya) ++ "\n</TD>"
     where
       trya :: String
       trya = if (onlyignorechars a) then "### Error: Empty cell not allowed in dot!" else a


   dotfont :: String -> String -> String
   dotfont opts a  = a --"\n<FONT"  ++ opts ++">" ++ (indent 2 trya) ++ "</FONT>"   ??? DIT WERKT NIET???
     where
       trya :: String
       trya = if (onlyignorechars a) then "### Error: Empty FONT not allowed in dot!" else a

   emptydottable :: String -> String
   emptydottable opts =  "\n<TABLE" ++ opts ++ "><TR><TD BGCOLOR=\"white\"><FONT COLOR=\"white\">.</FONT></TD></TR></TABLE>"
   notableborderopts :: String
   notableborderopts  = " BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\""
   indent :: Int -> String -> String
   indent n "" = ""
   indent n ('\n':str) = "\n" ++ (spaces n) ++ (indent n str)
   indent n (c:str) = c : indent n str

   spaces :: Int -> String
 --  spaces 0 = ""
 --  spaces n | n > 0  = " " ++ (spaces (n-1))
   spaces n = [ ' ' | i<-[1..n]]
   onlyignorechars:: String -> Bool
   onlyignorechars "" = True
   onlyignorechars (c:cs) =
     case c of
       ' '  -> onlyignorechars cs
       '\n' -> onlyignorechars cs
       _    -> False

   -- cellen :: Int -> [(String, String)]
   -- cellen n = [ cel i | i <- [1..n]]

   

 
   --data Opt = Topt
   data Topts = Tops [DotOpt]
   data Ropts = Rops [DotOpt]
   data Copts = Cops [DotOpt]
   type DotOpt = String

   data DotHtmlTable = DTable Topts [DotHtmlRow]
   data DotHtmlRow   = DRow  Ropts [DotHtmlCell]
   data DotHtmlCell  = DCell Copts String
 
 

   dottableopmaak :: DotHtmlTable -> String
   dottableopmaak (DTable opts dotrows)
       =  "<TABLE" ++ (optopmaak opts) ++ ">" ++
             (indent 2 ("\n" ++ (chain "\n" (map dotrowopmaak dotrows)))) ++
          "\n</TABLE>"

   dotrowopmaak :: DotHtmlRow -> String
   dotrowopmaak (DRow opts dotcells)
       =  "<TR" ++ (optopmaak opts) ++ ">" ++
             (indent 2 ("\n" ++ (chain "\n" (map dotcelopmaak dotcells)))) ++
          "\n</TR>"
 
   dotcelopmaak :: DotHtmlCell -> String
   dotcelopmaak (DCell opts str)
       =  indent 0 ( "<TD" ++ (optopmaak opts) ++ ">\n" ++
                        str ++ "\n" ++
                     "</TD>")


   optopmaak dotopts = "" -- TODO HAN. De opmaak van opties moet nog worden geregeld.

   dcell :: Int -> DotHtmlCell
   dcell n =  DCell opts str
      where
         opts = Cops []              --[(" Copt " ++ show n)]
         str = " Celinhoud " ++ show n

   drow :: Int -> DotHtmlRow
   drow n = DRow opts cells
      where
         opts = Rops []  --(" Ropt " ++ show n)
         cells = [ dcell i | i <- [1..n]]

   dtablefortesting :: Int -> Int -> DotHtmlTable
   dtablefortesting n m = DTable opts rows
      where
         opts = Tops []            --(" Topt " ++ show n)
         rows = [ drow n | i <- [1..m]]

 

 
   --rows :: String -> [ String -> String]
   --rows = "rowoptions" [" optiecel1"  "cel 1" ]

   --dothtmlrow :: String -> [String -> String] -> String
   --dothtmlrow rowopts [] = dotrow rowopts (emptydottable notableborderopts)
   --dothtmlrow rowopts cells = " test " -- dotrow "" " arow "

   --dothtmltable :: String -> [String -> [String -> String]] -> String
   --dothtmltable topts [] =  emptydottable topts
   --dothtmltable topts rowitems = dottable topts  chainedrows
   --   where
   --     chainedrows :: String
   --     chainedrows = (chain "\n" aap) ++ "\n"
   --          where
   --            aap :: String -> String
   --            aap = (map dothtmlrow rows)
   --               where







   testCD
    = OOclassdiagram
      [ OOClass "Plan" [ooAttr "afkomst" "Actor"] []
      , OOClass "Formulier" [ooAttr "plan" "Plan",ooAttr "van" "Actor",ooAttr "aan" "Actor",ooAttr "sessie" "Sessie"] []
      , OOClass "Dossier" [ooAttr "eigenaar" "Actor"] []
      , OOClass "Gegeven" [ooAttr "type" "Gegevenstype",ooAttr "in" "Dossier",ooAttr "veldnaam" "Veldnaam",ooAttr "waarde" "Waarde"] []
      , OOClass "Veld" [ooAttr "type" "Veldtype",ooAttr "waarde" "Waarde"] []
      , OOClass "Veldtype" [ooAttr "veldnaam" "Veldnaam",ooAttr "formuliertype" "Plan",ooAttr "gegevenstype" "Gegevenstype"] []
      , OOClass "Sessie" [ooAttr "dossier" "Dossier",ooAttr "uitgevoerd" "Actor"] []
      ]
      [ OOAssoc "Plan" "0..n" "" "Plan" "0..n" "stap"
      , OOAssoc "Formulier" "0..n" "" "Actor" "0..n" "inzage"
      , OOAssoc "Formulier" "0..n" "" "Formulier" "0..n" "in"
      , OOAssoc "Formulier" "0..n" "" "Plan" "0..n" "stap"
      , OOAssoc "Autorisatie" "0..n" "" "Actor" "0..n" "aan"
      , OOAssoc "Gegeven" "0..n" "" "Formulier" "0..n" "op"
      , OOAssoc "Gegeven" "0..n" "" "Actor" "0..n" "inzage"
      , OOAssoc "Actor" "0..n" "" "Actor" "0..n" "gedeeld"
      , OOAssoc "Formulier" "0..n" "" "Actor" "0..n" "inzagerecht"
      , OOAssoc "Gegeven" "0..n" "" "Actor" "0..n" "inzagerecht"
      , OOAssoc "Autorisatie" "0..n" "" "Gegeven" "0..n" "object"
      , OOAssoc "Actie" "0..n" "" "Gegeven" "0..n" "object"
      , OOAssoc "Autorisatie" "0..n" "" "Actie" "0..n" "op"
      , OOAssoc "Autorisatie" "0..n" "" "Actor" "0..n" "door"
      , OOAssoc "Actie" "0..n" "" "Actor" "0..n" "door"
      , OOAssoc "Veld" "0..n" "" "Gegeven" "0..n" "bindt"
      , OOAssoc "Sessie" "0..1" "" "Actor" "0..1" "actief"
      , OOAssoc "Formulier" "0..n" "" "Actor" "0..n" "openstaand"
      , OOAssoc "Gegeven" "0..n" "" "Actor" "0..n" "openstaand"
      ]
      [ OOAggr Close "Dossier" "Formulier"
      , OOAggr Close "Formulier" "Veld"
      ]
      []
      ("NoPat",[])
      where ooAttr nm t = OOAttr nm t True