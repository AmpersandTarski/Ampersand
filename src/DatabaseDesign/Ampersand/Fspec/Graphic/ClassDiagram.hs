{-# OPTIONS_GHC -Wall #-}
--TODO -> clean and stuff. Among which moving classdiagram2dot to Graphviz library implementation (see Classes/Graphics.hs).
--        I only helped it on its feet and I have put in the fSpec, now it generates stuff. I like stuff :)

module DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram
         (ClassDiag(..), Class(..), Attribute(..), Association(..), Aggregation(..), Generalization(..), Deleting(..), Method(..),
          Multiplicities(..), MinValue(..), MaxValue(..),
          clAnalysis, plugs2classdiagram, cdAnalysis, classdiagram2dot)
where
   import Data.Char (isAlphaNum,ord,isUpper,toUpper)
   import Data.List
   import DatabaseDesign.Ampersand.Basics
   import DatabaseDesign.Ampersand.Classes
   import DatabaseDesign.Ampersand.ADL1  hiding (Association)
   import DatabaseDesign.Ampersand.Fspec.Plug
   import DatabaseDesign.Ampersand.Misc
   import DatabaseDesign.Ampersand.Fspec.Fspec
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.Graphic.ClassDiagram"

--   import Data.ClassDiag  
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
    nodes (OOclassdiagram cs as rs gs _) = nub (concat (map nodes cs++map nodes as++map nodes rs++map nodes gs))

   instance CdNode Class where
    nodes (OOClass c _ _) = [c]
   instance CdNode a => CdNode [a] where
    nodes = concatMap nodes

   instance CdNode Attribute where
    nodes (OOAttr _ t _) = [t]

   instance CdNode Method where
    nodes _ = []

   instance CdNode Association where
    nodes (OOAssoc s _ _ t _ _) = [s,t]

   instance CdNode Aggregation where
    nodes (OOAggr _ s t) = [s,t]

   instance CdNode Generalization where
    nodes (OOGener g ss) = g:ss

-- The following function, clAnalysis, makes a classification diagram.
-- It focuses on generalizations and specializations.
   clAnalysis :: Fspc -> Options -> Maybe ClassDiag
   clAnalysis fSpec _ = if null classes' then Nothing else Just (OOclassdiagram classes' [] [] geners' ("classification"++name fSpec, concs fSpec))
    where
-- The following code was inspired on ADL2Plug
-- The first step is to determine which entities to generate.
-- All concepts and relations mentioned in exclusions are excluded from the process.
       rels       = [ERel (makeRelation rel) | rel@Sgn{} <- declarations fSpec, decusr rel, not (isIdent rel)]
       relsAtts   = [r | e<-rels, r<-[e, flp e], isUni r]
       cpts       = nub [ c
                        | gs<-fsisa fSpec
                        , let c=fst gs -- select only those generalisations whose specific concept is part of the themes to be printed.
                        , null (themes fSpec) || c `elem` (concs [mors pat | pat<-patterns fSpec, name pat `elem` themes fSpec ] `uni`  -- restrict to those themes that must be printed.
                                                           concs [mors (fpProc prc) | prc<-vprocesses fSpec, name prc `elem` themes fSpec ])
                        , (not.null) [ r | r<-relsAtts, source r==c ] ||  -- c is either a concept that has attributes or
                               null  [ r | r<-relsAtts, target r==c ]     --      it does not occur as an attribute.
                        ]
       geners'    = nub [ OOGener (name c) (map (name.snd) gs)
                        | gs<-eqCl fst (fsisa fSpec)
                        , let c=fst (head gs), c `elem` cpts -- select only those generalisations whose specific concept is part of the themes to be printed.
                        ]
       classes'   = [ OOClass (name c) (attrs c) []
                    | c<-cpts]
       attrs c    = [ OOAttr (fldname fld) (if isPropty fld then "Bool" else  name (target (fldexpr fld))) (fldnull fld)
                    | plug<-lookup' c, fld<-tail (tblfields plug), not (inKernel fld), source (fldexpr fld)==c]
                    where inKernel fld = null([Uni,Inj,Sur]>-multiplicities (fldexpr fld)) && not (isPropty fld)
       lookup' c = [p |InternalPlug p@(TblSQL{})<-plugInfos fSpec , (c',_)<-cLkpTbl p, c'==c]
       isPropty fld = null([Sym,Asy]>-multiplicities (fldexpr fld))
        
-- The following function, plugs2classdiagram, is useful to make a technical data model.
-- It draws on the plugs, which are meant to implement database tables for OLTP purposes.
-- Plugs come in three flavours: TblSQL, which is an entity (class),
--                               BinSQL, which is a relation between entities, and
--                               ScalarSQL, which represents scalars.
   plugs2classdiagram :: Fspc -> Options -> ClassDiag
   plugs2classdiagram fSpec _ = OOclassdiagram classes' assocs' aggrs' geners' (name fSpec, concs fSpec)
    where
-- The condition for becoming a class is in the function isClass. Does this correspond with the distinction TblSQL and BinSQL?
       isClass  :: PlugSQL -> Bool
       isClass  p = (not.null) [fld |fld<-tblfields p, flduniq fld] &&      -- an assocciation does not have fields that are flduniq
                    (not.null) [fld |fld<-tblfields p, not (flduniq fld)]   -- a scalar has only fields that are flduniq
       classes'   = [ OOClass (name (concept plug)) [ OOAttr a atype fNull | (a,atype,fNull)<-attrs plug] [] -- drop the I field.
                    | InternalPlug plug <- plugInfos fSpec, isClass plug
                    , not (null (attrs plug))
                    ]
       assocs'    = [ OOAssoc (nm source s) (mults $ EFlp rel) "" (nm target t) (mults rel) relname
                    | InternalPlug plug@(BinSQL{}) <-plugInfos fSpec
                    , let rel=mLkp plug
                    , not ((isSignal.head.mors) rel)
                    , let relname=case rel of
                           ERel r -> name r
                           EFlp (ERel r) -> name r
                           _ -> fatal 109 (show rel ++ " has no name.")
                    , let (s,t)=columns plug
                    ]
                    where
                     mults r = let minVal = if isTot r then MinOne else MinZero
                                   maxVal = if isInj r then MaxOne else MaxMany
                               in  Mult minVal maxVal 
                     nm f = name.concept.lookup'.f.fldexpr
       aggrs'     = []
       geners'    = []
       -- The attributes are shown without the key-attributes. Hence the first attribute (key of this concept) and
       -- the keys of its subtypes (filtered by 'inKernel') are not shown.
       attrs plug = [ if isPropty fld
                      then (fldname fld, "Bool",                      False      )
                      else (fldname fld, name (target (fldexpr fld)), fldnull fld)
                    | fld<-tail (tblfields plug), not (inKernel fld)]
                    where isPropty fld = null([Sym,Asy]>-multiplicities (fldexpr fld))
                    -- TODO: (SJ) I'm not sure if inKernel is correct. Check with Bas.
                          inKernel fld = null([Uni,Inj,Sur]>-multiplicities (fldexpr fld)) && not (isPropty fld)
       lookup' c = if null ps
                   then fatal 112 $ "erroneous lookup for concept "++name c++" in plug list"
                   else head ps
                   where ps = [p |InternalPlug p<-plugInfos fSpec, case p of ScalarSQL{} -> c==cLkp p; _ -> c `elem` [c' |(c',_)<-cLkpTbl p, c'==c]]

-- The following function, cdAnalysis, generates a conceptual data model.
-- It creates a class diagram in which generalizations and specializations remain distinct entities.
-- This yields more classes than plugs2classdiagram does, as plugs contain their specialized concepts.
-- Properties and identities are not shown.
   cdAnalysis :: Fspc -> Options -> ClassDiag
   cdAnalysis fSpec _ = OOclassdiagram classes' assocs' aggrs' geners' (name fSpec, concs fSpec)
    where
       classes'   = let cls=eqClass (==) assRels in
                    if length cls /= length assRels
                    then fatal 125 (show [map show cl | cl<-cls, length cl>1])
                    else [ OOClass (name c) (attrs cl) []
                         | cl<-eqCl source attRels, let c=source (head cl)
                         , c `elem` (map source assRels `uni` map target assRels)]
       assocs'    = [ OOAssoc (name (source r)) (mults $ EFlp r) "" (name (target r)) (mults r) ((name.head.morlist) r)
                    | r<-assRels]
                    where
                     mults r = let minVal = if isTot r then MinOne else MinZero
                                   maxVal = if isInj r then MaxOne else MaxMany
                               in  Mult minVal maxVal 
       aggrs'     = []
       geners'    = []
-- The following code was inspired on ADL2Plug
-- The first step is to determine which entities to generate.
-- All concepts and relations mentioned in exclusions are excluded from the process.
       rels       = [ERel (makeRelation rel) | rel@Sgn{} <- declarations fSpec, decusr rel, not (isIdent rel)]
       relsLim    = [ERel (makeRelation rel)           -- The set of relations that is defined in patterns to be printed.
                    | rel@Sgn{} <- declarations fSpec
                    , null (themes fSpec) || decpat rel `elem` themes fSpec   -- restrict to those themes that must be printed.
                    , decusr rel, not (isIdent rel)]
-- In order to make classes, all relations that are univalent and injective are flipped
-- attRels contains all relations that occur as attributes in classes.
       attRels    = [r |r<-rels, isUni r, not (isInj r)]        ++[EFlp r |r<-rels, not (isUni r), isInj r] ++
                    [r |r<-rels, isUni r,      isInj r, isSur r]++[EFlp r |r<-rels,      isUni r , isInj r, not (isSur r)]
-- assRels contains all relations that do not occur as attributes in classes
       assRels    = [r |r<-relsLim, not (isUni r), not (isInj r)]
       attrs rs   = [ OOAttr ((name.head.morlist) r) (name (target r)) (not(isTot r))
                    | r<-rs, not (isPropty r)]
       isPropty r = null([Sym,Asy]>-multiplicities r)

   classdiagram2dot :: Options -> ClassDiag -> String
   classdiagram2dot flags cd@(OOclassdiagram cs' as' rs' gs' (_, concspat))
            = "digraph G {rankdir=LR;bgcolor=transparent\n" ++        
              "    edge [ \n" ++
              "            fontsize = 11"++(if layout=="neato" then ", len = 3" else "")++" \n" ++
              "    ]\n" ++
              classes2dot cs' (nodes cd>-nodes cs')++ "\n" ++
              associations2dot as' ++ "\n" ++
              aggregations2dot rs' ++ "\n" ++
              generalizations2dot gs' ++
              "\n}\n"
          where
          layout = "dot"
          classes2dot :: [Class] -> [String] -> String
          classes2dot cs os
           = defaultclass ++
             (if null cs then "" else '\n': intercalate "\n" (map class2dot cs))++
             (if null os then "" else '\n': intercalate "\n" (map clas2dot os))
             where defaultclass = "    Node [shape = box] \n"
          clas2dot :: String -> String
          clas2dot n = spaces 5 ++ alias n ++ " [shape=box label=\""++n++"\"]"
          class2dot :: Class -> String
          class2dot (OOClass n' as'' ms') = spaces 5 ++ alias n' ++ " [\n" ++
                                           spaces 7 ++ "shape=plaintext \n" ++
                                                    classlabel n' as'' ms' ++
                                                    "\n     ]"
            where
              classlabel n as ms = spaces 7 ++ "label =<" ++
                                     indent 10 (dottable tableopts tablecontent) ++ "\n" ++
                                   spaces 7 ++ ">"
                    where
                      tableopts = " BGCOLOR=\"white\" BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\""
                      tablecontent =  dotrow "" (dotcell " BGCOLOR=\"lightgray\" ALIGN=\"center\""
                                                          (dotfont (if blackWhite flags then "" else " COLOR=\"red\"")
                                                                   n)) ++
                                      (if null as && null ms then "" else dotrow "" (dotcell "" (attribs2dot as))) ++
                                      (if null ms then "" else dotrow "" (dotcell "" (methods2dot ms)))

              attribs2dot :: [Attribute] -> String
              attribs2dot [] = emptydottable notableborderopts
              attribs2dot as = dottable notableborderopts (intercalate "" (map attrib2dot as))
 
              attrib2dot :: Attribute -> String
              attrib2dot (OOAttr n t fNull) = dotrow "" (dotcell " ALIGN=\"left\"" ((if fNull then "o " else "+ ") ++ n ++ " : " ++ t))

              methods2dot :: [Method] -> String
              methods2dot [] = emptydottable notableborderopts
              methods2dot ms = dottable notableborderopts (intercalate "" (map method2dot ms))

              method2dot :: Method -> String
              method2dot m =  dotrow "" (dotcell " ALIGN=\"left\"" ("+ " ++ show m ))

  -------------------------------
  --        ASSOCIATIONS:      --
  -------------------------------
          alias nm
           = if map toUpper nm `elem` ["NODE", "EDGE"]
             then (enc True . head) [ nm++show (i::Int) | i<-[1..], nm++show i `notElem` map name concspat]
             else enc True nm
          associations2dot :: [Association] -> String
          associations2dot as = intercalate "\n" (map association2dot as) ++ "\n"
          association2dot :: Association -> String
          association2dot (OOAssoc from m1 _ to m2 n2) =
              "      edge [ \n" ++
              "              arrowhead = \"none\" \n" ++
              "              arrowtail = \"none\" \n" ++
              (if null (nametable $ mult2Str m2) then "" else "              headlabel = " ++ nametable (mult2Str m2) ++ "\n") ++
              (if null (nametable $ mult2Str m1) then "" else "              taillabel = " ++ nametable (mult2Str m1) ++ "\n") ++
              "              label = \"" ++ n2 ++ "\" \n" ++
              "      ]\n" ++
              "       " ++ alias from ++ " -> " ++ alias to
              where
                 mult2Str (Mult MinZero MaxOne)  = "[0..1]"
                 mult2Str (Mult MinZero MaxMany) = "[0..n]"
                 mult2Str (Mult MinOne  MaxOne)  = "[1..1]"
                 mult2Str (Mult MinOne  MaxMany) = "[1..n]"
                  
                 nametable "" = "\"\""
                 nametable name' = dothtml (dottable notableborderopts 
                                              (dotrow "" (dotcell "" name')))


  -------------------------------
  --        AGGREGATIONS:      --
  -------------------------------
          aggregations2dot :: [Aggregation] -> String
          aggregations2dot rs = intercalate "\n" (map aggregation2dot rs) ++ "\n"
          aggregation2dot :: Aggregation -> String
 
          aggregation2dot (OOAggr del from to) =
              "      edge [ \n" ++
              "              headlabel = \"\"\n"    ++
              "              taillabel = \"\"\n"    ++
              "              arrowtail = " ++ aTail del ++" \n" ++
              "              arrowhead = \"none\" \n" ++
              "              label =\"\"" ++
              "      ]\n" ++
              "       " ++ alias from ++ " -> " ++ alias to
              where
                 aTail Open  = "\"odiamond\""
                 aTail Close = "\"diamond\""
 

 
  -------------------------------
  --        GENERALIZATIONS:   --       -- Ampersand statements such as "GEN Dolphin ISA Animal" are called generalization.
  --                           --       -- Generalizations are represented by a red arrow with a (larger) open triangle as arrowhead 
  -------------------------------
          generalizations2dot :: [Generalization] -> String
          generalizations2dot gs = intercalate "\n" (map generalization2dot gs) ++ "\n"
 
          generalization2dot :: Generalization -> String
          generalization2dot (OOGener _ []) = ""
          generalization2dot (OOGener a subs) = genEdge a firstsub ++ generalization2dot (OOGener a restsubs)
           where
             firstsub = head subs
             restsubs = tail subs
             genEdge a' b =
              "      edge [ \n" ++
              "              headlabel = \"\"\n"    ++
              "              taillabel = \"\"\n"    ++
              "              arrowtail = \"none\" \n" ++
              "              arrowhead = onormal \n" ++
              "              arrowsize = 2.0 \n" ++
              "              " ++( if blackWhite flags
                                   then "              style = dashed"
                                   else "              color = red"
                                 ) ++
              "              label =\"\"" ++
              "      ]\n" ++
              "       " ++ alias a' ++ " -> " ++ alias b ++ "\n"

   dothtml :: String -> String
   dothtml content = "<\n  " ++ content ++ "\n>"

   dottable :: String -> String -> String
     -- This function is designed to keep in mind that Graphviz does not cope with
     -- empty tables. In case the table has no content, the TABEL taggs are filled
     -- with a single dotrow with no contents.
   dottable opts a = "\n<TABLE" ++ opts ++">" ++ indent 2 trya ++ "\n</TABLE>"
     where
       trya :: String
       trya  = if onlyignorechars a then dotrow "" "" else a

   dotrow :: String -> String -> String
     -- This function is designed to keep in mind that Graphviz does not cope with
     -- empty rows. In case the row has no content, the taggs are filled
     -- with a single dotcell with no contents.
   dotrow opts a   = "\n<TR"    ++ opts ++">" ++ indent 2 trya ++ "\n</TR>"
     where
       trya :: String
       trya  = if onlyignorechars a then dotcell "" "" else a

   dotcell :: String -> String -> String
   dotcell opts a  = "\n<TD"    ++ opts ++">" ++ indent 2 trya ++ "\n</TD>"
     where
       trya :: String
       trya = if onlyignorechars a then "### Error: Empty cell not allowed in dot!" else a


   dotfont :: String -> String -> String
   dotfont _ a  = a --"\n<FONT"  ++ opts ++">" ++ (indent 2 trya) ++ "</FONT>"   ??? DIT WERKT NIET???

   emptydottable :: String -> String
   emptydottable opts =  "\n<TABLE" ++ opts ++ "><TR><TD BGCOLOR=\"white\"><FONT COLOR=\"white\">.</FONT></TD></TR></TABLE>"
   notableborderopts :: String
   notableborderopts  = " BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\""
   indent :: Int -> String -> String
   indent _ "" = ""
   indent n ('\n':str) = "\n" ++ spaces n ++ indent n str
   indent n (c:str) = c : indent n str

   onlyignorechars:: String -> Bool
   onlyignorechars "" = True
   onlyignorechars (c:cs) =
     case c of
       ' '  -> onlyignorechars cs
       '\n' -> onlyignorechars cs
       _    -> False

   -- cellen :: Int -> [(String, String)]
   -- cellen n = [ cel i | i <- [1..n]]

   

 

-------------- Class Diagrams ------------------
   data ClassDiag = OOclassdiagram {classes     :: [Class]            --
                                   ,assocs      :: [Association]      --
                                   ,aggrs       :: [Aggregation]      --
                                   ,geners      :: [Generalization]   --
                                   ,nameandcpts :: (String,[A_Concept])}
                            deriving Show
   instance Identified ClassDiag where
      name cd = n
        where (n,_) = nameandcpts cd
        
   data Class          = OOClass        String             --
                                        [Attribute]        --
                                        [Method]           --
                                    deriving Show
   data Attribute      = OOAttr         String             -- name of the attribute
                                        String             -- type of the attribute (Concept name or built-in type)
                                        Bool               -- fNull:  says whether the attribute is optional
                                    deriving Show
   
   data MinValue = MinZero | MinOne deriving (Show, Eq)
   
   data MaxValue = MaxOne | MaxMany deriving (Show, Eq)
   
   data Multiplicities = Mult MinValue MaxValue deriving Show
   
   data Association    = OOAssoc        String             -- source: the left hand side class
                                        Multiplicities     -- left hand side multiplicities
                                        String             -- left hand side role
                                        String             -- target: the right hand side class
                                        Multiplicities     -- right hand side multiplicities
                                        String             -- right hand side role
                                    deriving Show
   data Aggregation    = OOAggr         Deleting           --
                                        String             --
                                        String             --
                                    deriving (Show, Eq)
   data Generalization = OOGener        String             --
                                        [String]           --
                                    deriving (Show, Eq)

   data Deleting       = Open | Close                      --
                                    deriving (Show, Eq)
   data Method         = OOMethodC      String             -- name of this method, which creates a new object (producing a handle)
                                        [Attribute]        -- list of parameters: attribute names and types
                       | OOMethodR      String             -- name of this method, which yields the attribute values of an object (using a handle).
                                        [Attribute]        -- list of parameters: attribute names and types
                       | OOMethodS      String             -- name of this method, which selects an object using key attributes (producing a handle).
                                        [Attribute]        -- list of parameters: attribute names and types
                       | OOMethodU      String             -- name of this method, which updates an object (using a handle).
                                        [Attribute]        -- list of parameters: attribute names and types
                       | OOMethodD      String             -- name of this method, which deletes an object (using nothing but a handle).
                       | OOMethod       String             -- name of this method, which deletes an object (using nothing but a handle).
                                        [Attribute]        -- list of parameters: attribute names and types
                                        String             -- result: a type

   instance Show Method where
    showsPrec _ (OOMethodC nm cs)  = showString (nm++"("++intercalate "," [ n | OOAttr n _ _<-cs]++"):handle")
    showsPrec _ (OOMethodR nm as)  = showString (nm++"(handle):["++intercalate "," [ n | OOAttr n _ _<-as]++"]")
    showsPrec _ (OOMethodS nm ks)  = showString (nm++"("++intercalate "," [ n | OOAttr n _ _<-ks]++"):handle")
    showsPrec _ (OOMethodD nm)     = showString (nm++"(handle)")
    showsPrec _ (OOMethodU nm cs)  = showString (nm++"(handle,"++intercalate "," [ n | OOAttr n _ _<-cs]++")")
    showsPrec _ (OOMethod nm cs r) = showString (nm++"("++intercalate "," [ n | OOAttr n _ _<-cs]++"): "++r)

--
--   testCD
--    = OOclassdiagram
--      [ OOClass "Plan" [ooAttr "afkomst" "Actor"] []
--      , OOClass "Formulier" [ooAttr "plan" "Plan",ooAttr "van" "Actor",ooAttr "aan" "Actor",ooAttr "sessie" "Sessie"] []
--      , OOClass "Dossier" [ooAttr "eigenaar" "Actor"] []
--      , OOClass "Gegeven" [ooAttr "type" "Gegevenstype",ooAttr "in" "Dossier",ooAttr "veldnaam" "Veldnaam",ooAttr "waarde" "Waarde"] []
--      , OOClass "Veld" [ooAttr "type" "Veldtype",ooAttr "waarde" "Waarde"] []
--      , OOClass "Veldtype" [ooAttr "veldnaam" "Veldnaam",ooAttr "formuliertype" "Plan",ooAttr "gegevenstype" "Gegevenstype"] []
--      , OOClass "Sessie" [ooAttr "dossier" "Dossier",ooAttr "uitgevoerd" "Actor"] []
--      ]
--      [ OOAssoc "Plan" "0..n" "" "Plan" "0..n" "stap"
--      , OOAssoc "Formulier" "0..n" "" "Actor" "0..n" "inzage"
--      , OOAssoc "Formulier" "0..n" "" "Formulier" "0..n" "in"
--      , OOAssoc "Formulier" "0..n" "" "Plan" "0..n" "stap"
--      , OOAssoc "Autorisatie" "0..n" "" "Actor" "0..n" "aan"
--      , OOAssoc "Gegeven" "0..n" "" "Formulier" "0..n" "op"
--      , OOAssoc "Gegeven" "0..n" "" "Actor" "0..n" "inzage"
--      , OOAssoc "Actor" "0..n" "" "Actor" "0..n" "gedeeld"
--      , OOAssoc "Formulier" "0..n" "" "Actor" "0..n" "inzagerecht"
--      , OOAssoc "Gegeven" "0..n" "" "Actor" "0..n" "inzagerecht"
--      , OOAssoc "Autorisatie" "0..n" "" "Gegeven" "0..n" "object"
--      , OOAssoc "Actie" "0..n" "" "Gegeven" "0..n" "object"
--      , OOAssoc "Autorisatie" "0..n" "" "Actie" "0..n" "op"
--      , OOAssoc "Autorisatie" "0..n" "" "Actor" "0..n" "door"
--      , OOAssoc "Actie" "0..n" "" "Actor" "0..n" "door"
--      , OOAssoc "Veld" "0..n" "" "Gegeven" "0..n" "bindt"
--      , OOAssoc "Sessie" "0..1" "" "Actor" "0..1" "actief"
--      , OOAssoc "Formulier" "0..n" "" "Actor" "0..n" "openstaand"
--      , OOAssoc "Gegeven" "0..n" "" "Actor" "0..n" "openstaand"
--      ]
--      [ OOAggr Close "Dossier" "Formulier"
--      , OOAggr Close "Formulier" "Veld"
--      ]
--      []
--      ("NoPat",[])
--      where ooAttr nm t = OOAttr nm t True
