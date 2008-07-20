> module ClassDiagram where
>  import Char (toUpper)
>  import CommonClasses (  Identified(name)
>                        , Collection(empty, (>-)) 
>                       )
>  import Auxiliaries (rd, chain, eqCl, enc)
>  import Typology (Inheritance(Isa))
>  import CC_aux  
>             ( rules, Context(Ctx)
>             , Concept
>             , showADL, Morphical
>             , Language
>             , isProperty, target, concs, source
>             , declaration, declarations, keys
>             , flp, multiplicities, isa, isFunction, isFlpFunction
>             , Morphism(Mph)
>             , Prop(Sur,Inj)
>             , posNone, cpu, isSignal, mors, sign
>             )
>  import HtmlFilenames (fnContext)
>  import Graphic 
>           ( genGraphics)

>  cdModel contexts contextname
>   = putStr ("\nObject oriented analysis for "++name context++"\n")           >>
>     (writeFile (fnm++".dot"). cdDataModel context False layout) context >>   -- generate abbreviated class diagram  (pick True for full class diagram)
>     putStr (fnm++".dot written\n")>>
>     genGraphics fnm layout
>     where
>      layout  = "neato" -- nongravitational layout
>           -- = "dot"   -- vertical layout (default)
>      rs      = rules context
>      context = head ([c| c<-contexts, name c==contextname]++
>                      [Ctx (contextname++" is not defined") [] empty [] [] [] [] [] []])
>      shR r   = showADL r
>      fnm     = fnContext context++"_CD"

>  class CdNode a where
>   nodes :: a->[String]

>  data ClassDiag      = OOclassdiagram [Class]            --
>                                       [Association]      --
>                                       [Aggregation]      --
>                                       [Generalization]   --
>                                   deriving Show
>  instance CdNode ClassDiag where
>   nodes (OOclassdiagram cs as rs gs) = rd (concat (map nodes cs++map nodes as++map nodes rs++map nodes gs))

>  data Class          = OOClass        String             --
>                                       [Attribute]        --
>                                       [Method]           --
>                                   deriving Show
>  instance CdNode Class where
>   nodes (OOClass c as ms) = [c]
>  instance CdNode a => CdNode [a] where
>   nodes = concat.map nodes

>  data Attribute      = OOAttr         String             -- name of the attribute
>                                       String             -- type of the attribute (Concept name or built-in type)
>                                   deriving Show
>  instance CdNode Attribute where
>   nodes (OOAttr nm t) = [t]

>  data Method         = OOMethodC      Name               -- name of this method, which creates a new object (producing a handle)
>                                       [Attribute]        -- list of parameters: attribute names and types
>                      | OOMethodR      Name               -- name of this method, which yields the attribute values of an object (using a handle).
>                                       [Attribute]        -- list of parameters: attribute names and types
>                      | OOMethodS      Name               -- name of this method, which selects an object using key attributes (producing a handle).
>                                       [Attribute]        -- list of parameters: attribute names and types
>                      | OOMethodU      Name               -- name of this method, which updates an object (using a handle).
>                                       [Attribute]        -- list of parameters: attribute names and types
>                      | OOMethodD      Name               -- name of this method, which deletes an object (using nothing but a handle).
>                      | OOMethod       Name               -- name of this method, which deletes an object (using nothing but a handle).
>                                       [Attribute]        -- list of parameters: attribute names and types
>                                       String             -- result: a type
>  instance CdNode Method where
>   nodes m = []

>  instance Show Method where
>   showsPrec p (OOMethodC nm cs)  = showString (nm++"("++chain "," [ n | OOAttr n t<-cs]++"):handle")
>   showsPrec p (OOMethodR nm as)  = showString (nm++"(handle):["++chain "," [ n | OOAttr n t<-as]++"]")
>   showsPrec p (OOMethodS nm ks)  = showString (nm++"("++chain "," [ n | OOAttr n t<-ks]++"):handle")
>   showsPrec p (OOMethodD nm)     = showString (nm++"(handle)")
>   showsPrec p (OOMethodU nm cs)  = showString (nm++"(handle,"++chain "," [ n | OOAttr n t<-cs]++")")
>   showsPrec p (OOMethod nm cs r) = showString (nm++"("++chain "," [ n | OOAttr n t<-cs]++"): "++r)

>  data Association    = OOAssoc        String             -- source: the left hand side class
>                                       Mult               -- left hand side multiplicities
>                                       Name               -- left hand side role
>                                       String             -- target: the right hand side class
>                                       Mult               -- right hand side multiplicities
>                                       Name               -- right hand side role
>                                   deriving Show
>  instance CdNode Association where
>   nodes (OOAssoc s ml rl t mr rr) = [s,t]

>  data Aggregation    = OOAggr         Deleting           --
>                                       String             --
>                                       String             --
>                                   deriving (Show, Eq)
>  instance CdNode Aggregation where
>   nodes (OOAggr d s t) = [s,t]

>  data Generalization = OOGener        String             --
>                                       [String]           --
>                                   deriving (Show, Eq)
>  instance CdNode Generalization where
>   nodes (OOGener g ss) = g:ss

>  type Mult           = String                            --
>  type Name           = String                            --
>  data Deleting       = Open | Close                      --
>                                   deriving (Show, Eq)

>  cdAnalysis :: ( Morphical a, Language a ) => Context -> Bool -> a -> ClassDiag
>  cdAnalysis context full pat = OOclassdiagram classes assocs aggrs geners
>   where
>      classes    = [ OOClass (name c)
>                             [ OOAttr (name a) (if isProperty a then "Bool" else name (target a)) | a<-as]
>                             (if full then ms else [])
>                   | c<-used, c `elem` concs pat
>                     -- all attributes:
>                   , as<-[[a| a<-attributes, source a == c, not (isProperty a)]]
>                     -- all editable attributes:
>                   , cs<-[[ OOAttr (name a++if null [v| v<-as, name v==name a, v/=a] then "" else name (target a)) (name (target a)) | a<-as, not (declaration a `elem` comp)]]
>                     -- all methods:
>                   , ms<-[[ OOMethodC ("new"++name c) cs | not (null as) ] ++
>                          [ OOMethodR ("get"++name c) [ OOAttr (if null [v| v<-as, name v==name a, v/=a] then name a else name a++name (target a)) (name (target a)) | a<-as] | not (null as) ] ++
>                          [ OOMethodR ("sel"++name c++"_by_"++key) [ OOAttr (if null [v| v<-ks, name v==name a, v/=a] then name a else name a++name (target a)) (name (target a)) | a<-ks] | (e,key,ks)<-keys context, e==c, not (null key)] ++
>                          [ OOMethodD ("del"++name c) | not (null as) ] ++
>                          [ OOMethodU ("upd"++name c) cs | not (null cs) ] ++
>                          [ OOMethod  (name s) [] "Bool" | s<-scs, isProperty s, source s == c ]]
>                   , not (null as) || not (null ms)
>                   ]
>      assocs     = [ OOAssoc (name (source s)) (multiplicity s) "" (name (target s)) (multiplicity (flp s)) (name s)
>                   | s<-sps, not (s `elem` declarations attributes), s `elem` declarations pat, not (isProperty s)]
>                   where
>                    multiplicity s | Sur `elem` multiplicities s && Inj `elem` multiplicities s = "1"
>                                   |                                Inj `elem` multiplicities s = "0..1"
>                                   | Sur `elem` multiplicities s                                = "1..n"
>                                   | otherwise                                                  = ""
>      aggrs      = []
>      geners     = rd [ OOGener (name (fst (head gs))) (map (name.snd) gs)| Isa pcs cs<-[isa pat], gs<-eqCl fst pcs]
>      attributes = [     Mph (name s++if isFlpFunction s then "Fun" else "") posNone [] (source s,target s) True s | s<-scs, isFunction s] ++
>                   [flp (Mph (name s++if isFunction s then "Inv" else "") posNone [] (source s,target s) True s)| s<-scs, isFlpFunction s]
>  -- obsolete:     ++ [     Mph (name s) posNone [] (source s,target s) True s | s<-scs, isProperty s]
>                   
>      comp = rd [s| rule<-rules context, toExpr<-cpu rule, s<-declarations toExpr]  -- all computed relations
>      sps = [d|d<-declarations pat, not (isSignal d)]
>      scs = [d|d<-declarations context, not (isSignal d)]
>      ms = mors (rules context)
>      used :: [Concept]
>      used = rd (concs ms ++                                                                   -- involved in aggregations
>                 [c| Isa pcs cs<-[isa pat], (a,b)<-pcs, c<-[a,b]] ++                           -- involved in generalizations
>                 [c| s<-sps, not (s `elem` declarations attributes), (a,b)<-[sign s], c<-[a,b]]  -- involved in associations
>                )

>  shDataModel (OOclassdiagram cs as rs gs)
>   = "OOclassdiagram\n>     "++chain "\n>     "
>       [ lijstopmaak (map show cs),
>         lijstopmaak (map show as),
>         lijstopmaak (map show rs),
>         lijstopmaak (map show gs)]
>   where lijstopmaak [] = "[]"
>         lijstopmaak xs = "[ "++chain "\n>     , " xs++"\n>     ]"

>  cdDataModel :: Language pat => Context -> Bool -> String -> pat -> String
>  cdDataModel context full layout pat = classdiagram2dot (cdAnalysis context full pat)
>   where classdiagram2dot cd@(OOclassdiagram cs as rs gs)
>           = "digraph G {bgcolor=transparent\n" ++        
>     --        "    fontname = \"Courier\" \n" ++
>     --        "    fontsize = 18 \n" ++
>     --        "    node [ \n" ++
>     --        "            fontname = \"Courier\" \n" ++
>     --        "            fontsize = 8 \n" ++
>     --        "            shape = \"record\" \n" ++
>     --        "    ] \n" ++
>             "    edge [ \n" ++
>     --        "            fontname = \"Courier\" \n" ++
>             "            fontsize = 11"++(if layout=="neato" then ", len = 3" else "")++" \n" ++
>             "    ]\n" ++
>             classes2dot cs (nodes cd>-nodes cs)++ "\n" ++
>             associations2dot as ++ "\n" ++
>             aggregations2dot rs ++ "\n" ++
>             generalizations2dot gs ++
>             "\n}\n"


>         classes2dot :: [Class] -> [String] -> [Char]
>         classes2dot cs os
>          = defaultclass ++
>            (if null cs then "" else "\n" ++ chain "\n" (map class2dot cs))++
>            (if null os then "" else "\n" ++ chain "\n" (map clas2dot os))
>            where defaultclass = "    Node [shape = box] \n"
>         clas2dot :: String -> String
>         clas2dot n = spaces 5 ++ alias n ++ " [shape=box label=\""++n++"\"]"
>         class2dot :: Class -> [Char]
>         class2dot (OOClass n as ms) = spaces 5 ++ alias n ++ " [\n" ++
>                                       spaces 7 ++ "shape=plaintext \n" ++
>                                               (classlabel n as ms) ++
>                                                   "\n     ]"
>           where
>             classlabel n as ms = spaces 7 ++ "label =<" ++
>                                    (indent 10 (dottable tableopts tablecontent)) ++ "\n" ++
>                                  spaces 7 ++ ">"
>                   where
>                     tableopts = " BGCOLOR=\"white\" BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\""
>                     tablecontent =  (dotrow "" (dotcell " BGCOLOR=\"lightgray\" ALIGN=\"center\""
>                                                         (dotfont " COLOR=\"red\""
>                                                                  n))) ++
>                                     (if null as && null ms then "" else dotrow "" (dotcell "" (attribs2dot as))) ++
>                                     (if null ms then "" else dotrow "" (dotcell "" (methods2dot ms)))
        
>             attribs2dot :: [Attribute] -> [Char]
>             attribs2dot [] = emptydottable notableborderopts
>             attribs2dot as = (dottable notableborderopts (chain "" (map attrib2dot as)))
>
>             attrib2dot :: Attribute -> [Char]
>             attrib2dot (OOAttr n t) = dotrow "" (dotcell " ALIGN=\"left\"" ("+ " ++ n ++ " : " ++ t))
        
>             methods2dot :: [Method] -> [Char]
>             methods2dot [] = emptydottable notableborderopts
>             methods2dot ms = dottable notableborderopts (chain "" (map method2dot ms))
 
>             method2dot :: Method -> [Char]
>             method2dot m =  dotrow "" (dotcell " ALIGN=\"left\"" ("+ " ++ show m ))
 
> -------------------------------
> --        ASSOCIATIONS:      --
> -------------------------------
>         alias nm
>          = if map toUpper nm=="NODE" ||
>               map toUpper nm=="EDGE"
>            then (enc True . head) [ nm++show i | i<-[1..], not ((nm++show i) `elem` map name (concs pat) )]
>            else enc True nm
>         associations2dot :: [Association] -> [Char]
>         associations2dot as = chain "\n" (map association2dot as) ++ "\n"
>         association2dot :: Association -> [Char]
>         association2dot (OOAssoc from m1 n1 to m2 n2) =
>             "      edge [ \n" ++
>             "              arrowhead = \"none\" \n" ++
>             "              arrowtail = \"none\" \n" ++
>             (if null (nametable m1) then "" else "              headlabel = " ++ nametable m1 ++ "\n") ++
>             (if null (nametable m1) then "" else "              taillabel = " ++ nametable m2 ++ "\n") ++
>             "              label = \"" ++ n2 ++ "\" \n" ++
>             "      ]\n" ++
>             "       " ++ alias from ++ " -> " ++ alias to
>             where 
>                nametable "" = "\"\""
>                nametable name = dothtml (dottable notableborderopts 
>                                             (dotrow "" (dotcell "" name)))

 
> -------------------------------
> --        AGGREGATIONS:      --
> -------------------------------
>         aggregations2dot :: [Aggregation] -> [Char]
>         aggregations2dot rs = chain "\n" (map aggregation2dot rs) ++ "\n"
>         aggregation2dot :: Aggregation -> [Char]
>
>         aggregation2dot (OOAggr del from to) =
>             "      edge [ \n" ++
>             "              headlabel = \"\"\n"    ++
>             "              taillabel = \"\"\n"    ++
>             "              arrowtail = " ++ tail del ++" \n" ++
>             "              arrowhead = \"none\" \n" ++
>             "              label =\"\"" ++
>             "      ]\n" ++
>             "       " ++ alias from ++ " -> " ++ alias to
>             where
>                tail Open  = "\"odiamond\""
>                tail Close = "\"diamond\""
>
 
>
> -------------------------------
> --        GENERALIZATIONS:   --       -- TODO : Wat bedoelt stef hier mee?? Wat is de syntax?
> -------------------------------
>         generalizations2dot :: [Generalization] -> [Char]
>         generalizations2dot gs = chain "\n" (map generalization2dot gs) ++ "\n"
>
>         generalization2dot :: Generalization -> [Char]
>         generalization2dot (OOGener a []) = ""
>         generalization2dot (OOGener a subs) = (genEdge a firstsub) ++ (generalization2dot (OOGener a restsubs))
>          where
>            firstsub = head subs
>            restsubs = tail subs
>            genEdge a b =
>             "      edge [ \n" ++
>             "              headlabel = \"\"\n"    ++
>             "              taillabel = \"\"\n"    ++
>             "              arrowtail = \"none\" \n" ++
>             "              arrowhead = " ++ "onormal" ++" \n" ++
>             "              label =\"\"" ++
>             "      ]\n" ++
>             "       " ++ alias b ++ " -> " ++ alias a ++ "\n"
>

>  dothtml :: String -> String
>  dothtml content = "<\n  " ++ content ++ "\n>"

>  dottable :: String -> String -> String
>    -- This function is designed to keep in mind that Graphviz does not cope with
>    -- empty tables. In case the table has no content, the TABEL taggs are filled
>    -- with a single dotrow with no contents.
>  dottable opts a = "\n<TABLE" ++ opts ++">" ++ (indent 2 trya ) ++ "\n</TABLE>"
>    where
>      trya :: String
>      trya  = if (onlyignorechars a) then (dotrow "" "") else a
 
>  dotrow :: String -> String -> String
>    -- This function is designed to keep in mind that Graphviz does not cope with
>    -- empty rows. In case the row has no content, the taggs are filled
>    -- with a single dotcell with no contents.
>  dotrow opts a   = "\n<TR"    ++ opts ++">" ++ (indent 2 trya) ++ "\n</TR>"
>    where
>      trya :: String
>      trya  = if (onlyignorechars a) then (dotcell "" "") else a
 
>  dotcell :: String -> String -> String
>  dotcell opts a  = "\n<TD"    ++ opts ++">" ++ (indent 2 trya) ++ "\n</TD>"
>    where
>      trya :: String
>      trya = if (onlyignorechars a) then "### Error: Empty cell not allowed in dot!" else a
 

>  dotfont :: String -> String -> String
>  dotfont opts a  = a --"\n<FONT"  ++ opts ++">" ++ (indent 2 trya) ++ "</FONT>"   ??? DIT WERKT NIET???
>    where
>      trya :: String
>      trya = if (onlyignorechars a) then "### Error: Empty FONT not allowed in dot!" else a
 
>  emptydottable :: String -> String
>  emptydottable opts =  "\n<TABLE" ++ opts ++ "><TR><TD BGCOLOR=\"white\"><FONT COLOR=\"white\">.</FONT></TD></TR></TABLE>"
>  notableborderopts :: String
>  notableborderopts  = " BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\""
>  indent :: Int -> String -> String
>  indent n "" = ""
>  indent n ('\n':str) = "\n" ++ (spaces n) ++ (indent n str)
>  indent n (c:str) = c : indent n str
 
>  spaces :: Int -> String
>--  spaces 0 = ""
>--  spaces n | n > 0  = " " ++ (spaces (n-1))
>  spaces n = [ ' ' | i<-[1..n]]
>  onlyignorechars:: String -> Bool
>  onlyignorechars "" = True
>  onlyignorechars (c:cs) =
>    case c of
>      ' '  -> onlyignorechars cs
>      '\n' -> onlyignorechars cs
>      _    -> False
 
>  -- cellen :: Int -> [(String, String)]
>  -- cellen n = [ cel i | i <- [1..n]]
 
>  

>
>  --data Opt = Topt
>  data Topts = Tops [DotOpt]
>  data Ropts = Rops [DotOpt]
>  data Copts = Cops [DotOpt]
>  type DotOpt = String

>  data DotHtmlTable = DTable Topts [DotHtmlRow]
>  data DotHtmlRow   = DRow  Ropts [DotHtmlCell]
>  data DotHtmlCell  = DCell Copts String
>
>
 
>  dottableopmaak :: DotHtmlTable -> String
>  dottableopmaak (DTable opts dotrows)
>      =  "<TABLE" ++ (optopmaak opts) ++ ">" ++
>            (indent 2 ("\n" ++ (chain "\n" (map dotrowopmaak dotrows)))) ++
>         "\n</TABLE>"
 
>  dotrowopmaak :: DotHtmlRow -> String
>  dotrowopmaak (DRow opts dotcells)
>      =  "<TR" ++ (optopmaak opts) ++ ">" ++
>            (indent 2 ("\n" ++ (chain "\n" (map dotcelopmaak dotcells)))) ++
>         "\n</TR>"
>
>  dotcelopmaak :: DotHtmlCell -> String
>  dotcelopmaak (DCell opts str)
>      =  indent 0 ( "<TD" ++ (optopmaak opts) ++ ">\n" ++
>                       str ++ "\n" ++
>                    "</TD>")
 

>  optopmaak dotopts = "" -- TODO HAN. De opmaak van opties moet nog worden geregeld.
 
>  dcell :: Int -> DotHtmlCell
>  dcell n =  DCell opts str
>     where
>        opts = Cops []              --[(" Copt " ++ show n)]
>        str = " Celinhoud " ++ show n
 
>  drow :: Int -> DotHtmlRow
>  drow n = DRow opts cells
>     where
>        opts = Rops []  --(" Ropt " ++ show n)
>        cells = [ dcell i | i <- [1..n]]
 
>  dtablefortesting :: Int -> Int -> DotHtmlTable
>  dtablefortesting n m = DTable opts rows
>     where
>        opts = Tops []            --(" Topt " ++ show n)
>        rows = [ drow n | i <- [1..m]]
 
>
 
>
>  --rows :: String -> [ String -> String]
>  --rows = "rowoptions" [" optiecel1"  "cel 1" ]
 
>  --dothtmlrow :: String -> [String -> String] -> String
>  --dothtmlrow rowopts [] = dotrow rowopts (emptydottable notableborderopts)
>  --dothtmlrow rowopts cells = " test " -- dotrow "" " arow "
 
>  --dothtmltable :: String -> [String -> [String -> String]] -> String
>  --dothtmltable topts [] =  emptydottable topts
>  --dothtmltable topts rowitems = dottable topts  chainedrows
>  --   where
>  --     chainedrows :: String
>  --     chainedrows = (chain "\n" aap) ++ "\n"
>  --          where
>  --            aap :: String -> String
>  --            aap = (map dothtmlrow rows)
>  --               where







>  testCD
>   = OOclassdiagram
>     [ OOClass "Plan" [OOAttr "afkomst" "Actor"] []
>     , OOClass "Formulier" [OOAttr "plan" "Plan",OOAttr "van" "Actor",OOAttr "aan" "Actor",OOAttr "sessie" "Sessie"] []
>     , OOClass "Dossier" [OOAttr "eigenaar" "Actor"] []
>     , OOClass "Gegeven" [OOAttr "type" "Gegevenstype",OOAttr "in" "Dossier",OOAttr "veldnaam" "Veldnaam",OOAttr "waarde" "Waarde"] []
>     , OOClass "Veld" [OOAttr "type" "Veldtype",OOAttr "waarde" "Waarde"] []
>     , OOClass "Veldtype" [OOAttr "veldnaam" "Veldnaam",OOAttr "formuliertype" "Plan",OOAttr "gegevenstype" "Gegevenstype"] []
>     , OOClass "Sessie" [OOAttr "dossier" "Dossier",OOAttr "uitgevoerd" "Actor"] []
>     ]
>     [ OOAssoc "Plan" "0..n" "" "Plan" "0..n" "stap"
>     , OOAssoc "Formulier" "0..n" "" "Actor" "0..n" "inzage"
>     , OOAssoc "Formulier" "0..n" "" "Formulier" "0..n" "in"
>     , OOAssoc "Formulier" "0..n" "" "Plan" "0..n" "stap"
>     , OOAssoc "Autorisatie" "0..n" "" "Actor" "0..n" "aan"
>     , OOAssoc "Gegeven" "0..n" "" "Formulier" "0..n" "op"
>     , OOAssoc "Gegeven" "0..n" "" "Actor" "0..n" "inzage"
>     , OOAssoc "Actor" "0..n" "" "Actor" "0..n" "gedeeld"
>     , OOAssoc "Formulier" "0..n" "" "Actor" "0..n" "inzagerecht"
>     , OOAssoc "Gegeven" "0..n" "" "Actor" "0..n" "inzagerecht"
>     , OOAssoc "Autorisatie" "0..n" "" "Gegeven" "0..n" "object"
>     , OOAssoc "Actie" "0..n" "" "Gegeven" "0..n" "object"
>     , OOAssoc "Autorisatie" "0..n" "" "Actie" "0..n" "op"
>     , OOAssoc "Autorisatie" "0..n" "" "Actor" "0..n" "door"
>     , OOAssoc "Actie" "0..n" "" "Actor" "0..n" "door"
>     , OOAssoc "Veld" "0..n" "" "Gegeven" "0..n" "bindt"
>     , OOAssoc "Sessie" "0..1" "" "Actor" "0..1" "actief"
>     , OOAssoc "Formulier" "0..n" "" "Actor" "0..n" "openstaand"
>     , OOAssoc "Gegeven" "0..n" "" "Actor" "0..n" "openstaand"
>     ]
>     [ OOAggr Close "Dossier" "Formulier"
>     , OOAggr Close "Formulier" "Veld"
>     ]
>     []
