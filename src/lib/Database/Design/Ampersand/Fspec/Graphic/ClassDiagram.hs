{-# OPTIONS_GHC -Wall #-}
--TODO -> clean and stuff. Among which moving classdiagram2dot to Graphviz library implementation (see Classes/Graphics.hs).
--        I only helped it on its feet and I have put in the fSpec, now it generates stuff. I like stuff :)

module Database.Design.Ampersand.Fspec.Graphic.ClassDiagram
         (ClassDiag(..), Class(..), CdAttribute(..), Association(..), Aggregation(..), Generalization(..), Deleting(..), Method(..),
          Multiplicities(..), MinValue(..), MaxValue(..),
          clAnalysis, cdAnalysis, tdAnalysis, classdiagram2dot)
where
   import Data.List
   import Database.Design.Ampersand.Basics
   import Database.Design.Ampersand.Classes
   import Database.Design.Ampersand.ADL1  hiding (Association,Box)
   import Database.Design.Ampersand.Core.AbstractSyntaxTree hiding (Association)
   import Database.Design.Ampersand.Fspec.Plug
   import Database.Design.Ampersand.Misc
   import Database.Design.Ampersand.Fspec.Fspec
   import Database.Design.Ampersand.Fspec.Motivations
   import Data.String
   import Data.Maybe
   import Data.GraphViz.Types.Canonical hiding (attrs)
   import Data.GraphViz.Attributes.Complete as GVcomp
   import Data.GraphViz.Attributes as GVatt
   import Data.GraphViz.Attributes.HTML as Html

   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.Graphic.ClassDiagram"

   class CdNode a where
    nodes :: a->[String]

   instance CdNode ClassDiag where
    nodes cd = nub (concat (  map nodes (classes cd)
                            ++map nodes (assocs  cd)
                            ++map nodes (aggrs   cd)
                            ++map nodes (geners  cd)
                   )       )

   instance CdNode Class where
    nodes cl = [clName cl]
   instance CdNode a => CdNode [a] where
    nodes = concatMap nodes

   instance CdNode CdAttribute where
    nodes (OOAttr _ t _) = [t]

   instance CdNode Method where
    nodes _ = []

   instance CdNode Association where
    nodes a = [assSrc a,assTgt a]

   instance CdNode Aggregation where
    nodes (OOAggr _ s t) = map name [s,t]

   instance CdNode Generalization where
    nodes g = map name ((concs.genAgen) g)

   -- | This function makes the classification diagram.
   -- It focuses on generalizations and specializations.
   clAnalysis :: Fspc -> ClassDiag
   clAnalysis fSpec =
       OOclassdiagram { cdName  = "classification_"++name fSpec
                      , classes = [ OOClass { clName = name c
                                            , clcpt  = Just ( c , [p | p<-purposesDefinedIn fSpec (fsLang fSpec) c] )
                                            , clAtts = attrs c
                                            , clMths = []
                                            } | c<-cpts]
                      , assocs  = []
                      , aggrs   = []
                      , geners  = map OOGener (gensInScope fSpec)
                      , ooCpts  = concs fSpec
                      }

    where
       cpts       = concs (gensInScope fSpec)
       attrs c    = [ OOAttr (fldname fld) (if isPropty fld then "Bool" else  (name.target.fldexpr) fld) (fldnull fld)
                    | plug<-lookup' c, fld<-tail (plugFields plug), not (inKernel fld), source (fldexpr fld)==c]
                    where inKernel fld = null([Uni,Inj,Sur]>-multiplicities (fldexpr fld)) && not (isPropty fld)
       lookup' c = [plug |InternalPlug plug@TblSQL{}<-plugInfos fSpec , (c',_)<-cLkpTbl plug, c'==c]
       isPropty fld = null([Sym,Asy]>-multiplicities (fldexpr fld))

   -- | This function, cdAnalysis, generates a conceptual data model.
   -- It creates a class diagram in which generalizations and specializations remain distinct entity types.
   -- This yields more classes than plugs2classdiagram does, as plugs contain their specialized concepts.
   -- Properties and identities are not shown.
   cdAnalysis :: Fspc -> ClassDiag
   cdAnalysis fSpec =
     OOclassdiagram { cdName  = "logical_"++name fSpec
                    , classes =
                        [ OOClass{ clName = name root
                                 , clcpt  = Just ( root , [p | p<-purposesDefinedIn fSpec (fsLang fSpec) root] )
                                 , clAtts = map ooAttr ooClass
                                 , clMths = []
                                 }
                        | ooClass <- ooClasses, let root=source (head ooClass)]
                    , assocs  =
                        [ OOAssoc { assSrc = name $ source d
                                  , assSrcPort = name d
                                  , asslhm = mults . flp $ EDcD d
                                  , asslhr = ""
                                  , assTgt = name $ target d
                                  , assrhm = mults d
                                  , assrhr = name d
                                  , asspurp = purposesDefinedIn fSpec (fsLang fSpec) d
                                  , assmean = meaning (fsLang fSpec) d
                                  }
                        | d <- allDcls
                        , (not.isPropty) d
                        , target d `elem` (roots ++ concatMap (smallerConcepts (gens fSpec)) roots
                                                 ++ concatMap (largerConcepts  (gens fSpec)) roots) 
                        ]
                    , aggrs   = []
                    , geners  = map OOGener (gensInScope fSpec)
                    , ooCpts  = roots
                    }

    where
      ooAttr :: Expression -> CdAttribute
      ooAttr r = OOAttr { attNm = (name . head . relsMentionedIn) r
                        , attTyp = if isPropty r then "Bool" else (name.target) r
                        , attOptional = (not.isTot) r
                        }
      isPropty r = null([Sym,Asy]>-multiplicities r)
      mults r = let minVal = if isTot r then MinOne else MinZero
                    maxVal = if isUni r then MaxOne else MaxMany
                in  Mult minVal maxVal
      allDcls = [ d -- restricted to those themes that must be printed.
                | d@Sgn{} <- nub . concat $
                               [relsDefdIn p ++ relsMentionedIn p  | p <- pattsInScope fSpec ] ++
                               [relsDefdIn p ++ relsMentionedIn p  | p <- procsInScope fSpec ]
                , decusr d]
      attribs = map flipWhenInj (filter isAttribRel allDcls)
          where isAttribRel d = isUni d || isInj d
                flipWhenInj d = if isInj d then flp (EDcD d) else EDcD d
      ooClasses = eqCl source attribs      -- an equivalence class wrt source yields the attributes that constitute an OO-class.
      roots = map (source.head) ooClasses

   -- | This function generates a technical data model.
   -- It is based on the plugs that are calculated.
   tdAnalysis :: Fspc -> ClassDiag
   tdAnalysis fSpec =
     OOclassdiagram {cdName  = "technical_"++name fSpec
                    ,classes = allClasses
                    ,assocs  = allAssocs
                    ,aggrs   = []
                    ,geners  = []
                    ,ooCpts  = roots
                    }
     where
      allClasses =
         [ OOClass{ clName = sqlname table
                  , clcpt  = primKey table
                  , clAtts = case table of
                               TblSQL{fields=attribs, cLkpTbl=kernelLookupTbl, mLkpTbl=t} -> 
                                 let kernelFlds = map snd $ kernelLookupTbl -- extract kernel fields from kernel lookup table
                                 in  map (ooAttr kernelFlds . lookInFor t . fldexpr) attribs
                               BinSQL{columns=(a,b)}      ->
                                 [ OOAttr { attNm       = fldname a
                                          , attTyp      = (name.target.fldexpr) a
                                          , attOptional = False
                                          }
                                 , OOAttr { attNm       = fldname b
                                          , attTyp      = (name.target.fldexpr) b
                                          , attOptional = False
                                          }
                                 ]
                               _     -> fatal 166 "Unexpeced type of table!"
                  , clMths = []
                  }
         | table <- tables
         , length (plugFields table) > 1
         ]

      lookInFor [] _ = fatal 191 "Expression not found!"
      lookInFor ((expr,_,t):xs) a
                 | expr == a = t
                 | otherwise = lookInFor xs a
      tables = [ pSql | InternalPlug pSql <- plugInfos fSpec, not (isScalar pSql)]
         where isScalar ScalarSQL{} = True
               isScalar _           = False
      roots :: [A_Concept]
      roots = (map fst.catMaybes.map primKey) tables
      primKey :: PlugSQL -> Maybe (A_Concept, [Purpose])
      primKey TblSQL{fields=(f:_)} = Just (source (fldexpr f), [])  -- purposes are no longer available in the technical data model.
      primKey _                    = Nothing
      ooAttr :: [SqlField] -> SqlField -> CdAttribute
      ooAttr kernelFlds f =
        OOAttr { attNm = fldname f
               , attTyp = if null([Sym,Asy]>-multiplicities (fldexpr f)) && (f `notElem` kernelFlds)
                          then "Bool"
                          else (name.target.fldexpr) f
               , attOptional = fldnull f
               }
      allAssocs = filter isAssocBetweenClasses $ concatMap relsOf tables
        where
          isAssocBetweenClasses a = let allClassNames = map clName allClasses in assSrc a `elem` allClassNames && assTgt a `elem` allClassNames
          
          kernelConcepts = map fst (concatMap cLkpTbl tables)

          relsOf t =
            case t of
              TblSQL{} -> map (mkRel t) (catMaybes [relOf fld | fld <- fields t])
              BinSQL{columns=(a,b)} ->
                        [ OOAssoc { assSrc = sqlname t
                                  , assSrcPort = fldname a
                                  , asslhm = Mult MinZero MaxMany
                                  , asslhr = ""
                                  , assTgt = getConceptTableFor fSpec . target . fldexpr $ a
                                  , assrhm = Mult MinOne MaxOne
                                  , assrhr = ""
                                  , asspurp = []      -- in the technical data model, the purpose is not documented.
                                  , assmean = Nothing -- in the technical data model, the meaning is not documented.
                                  }
                        , OOAssoc { assSrc = sqlname t
                                  , assSrcPort = fldname b
                                  , asslhm = Mult MinZero MaxMany
                                  , asslhr = ""
                                  , assTgt = getConceptTableFor fSpec . target . fldexpr $ b
                                  , assrhm = Mult MinOne MaxOne
                                  , assrhr = ""
                                  , asspurp = []      -- in the technical data model, the purpose is not documented.
                                  , assmean = Nothing -- in the technical data model, the meaning is not documented.
                                  
                                  }
                        ]
              _  -> fatal 195 "Unexpected type of table"
          relOf f =
            let expr = fldexpr f in
            case expr of
              EDcI{} -> Nothing
              EDcD d -> if target d `elem` kernelConcepts then Just (expr,f) else Nothing
              EFlp (EDcD d) -> if source d `elem` kernelConcepts then Just (expr,f) else Nothing
              _ -> fatal 200 ("Unexpected expression: "++show expr)
          mkRel :: PlugSQL -> (Expression,SqlField) -> Association
          mkRel t (expr,f) =
               OOAssoc { assSrc = sqlname t
                       , assSrcPort = fldname f
                       , asslhm = (mults.flp) expr
                       , asslhr = fldname f
                       , assTgt = getConceptTableFor fSpec (target expr)
                       , assrhm = mults expr
                       , assrhr = case [name d | d@Sgn{}<-relsMentionedIn expr] of h:_ -> h ; _ -> fatal 229 "no relations used in expr"
                       , asspurp = [] -- in the technical data model, the purpose is not documented.
                       , assmean = Nothing -- in the technical data model, the meaning is not documented.
                       }
      mults r = let minVal = if isTot r then MinOne else MinZero
                    maxVal = if isUni r then MaxOne else MaxMany
                in  Mult minVal maxVal

---- In order to make classes, all relations that are univalent and injective are flipped
---- attRels contains all relations that occur as attributes in classes.
--       attRels    = [r |r<-rels, isUni r, not (isInj r)]        ++[flp r |r<-rels, not (isUni r), isInj r] ++
--                    [r |r<-rels, isUni r,      isInj r, isSur r]++[flp r |r<-rels,      isUni r , isInj r, not (isSur r)]
---- assRels contains all relations that do not occur as attributes in classes
--       assRels    = [r |r<-relsLim, not (isUni r), not (isInj r)]
--       attrs rs   = [ OOAttr ((name.head.relsMentionedIn) r) (name (target r)) (not(isTot r))
--                    | r<-rs, not (isPropty r)]
--       isPropty r = null([Sym,Asy]>-multiplicities r)

   -- | translate a ClassDiagram to a DotGraph, so it can be used to show it as a picture.
   classdiagram2dot :: Options -> ClassDiag -> DotGraph String
   classdiagram2dot opts cd
    = DotGraph { strictGraph     = False
               , directedGraph   = True
               , graphID         = Nothing
               , graphStatements = 
                       DotStmts
                           { attrStmts =  [ GraphAttrs [ RankDir FromLeft
                                                       , bgColor White]
                                          ]
                                   --    ++ [NodeAttrs  [ ]]
                                       ++ [EdgeAttrs  [ FontSize 11
                                                      , MinLen 4
                                          ]           ]
                           , subGraphs = []
                           , nodeStmts = allNodes (classes cd) (nodes cd >- nodes (classes cd))
                           , edgeStmts = (map association2edge (assocs cd))  ++
                                         (map aggregation2edge (aggrs cd))  ++
                                         (concatMap generalization2edges (geners cd))
                           }
               }
        where
          allNodes :: [Class] -> [String] -> [DotNode String]
          allNodes cs others =
             map class2node cs ++
             map nonClass2node others

          class2node :: Class -> DotNode String
          class2node cl = DotNode
            { nodeID         = name cl
            , nodeAttributes = [ Shape PlainText
                               , GVcomp.Color [WC (X11Color Purple) Nothing]
                               , Label (HtmlLabel (Table htmlTable))
                               ]
            } where
             htmlTable = HTable { tableFontAttrs = Nothing
                                , tableAttrs     = [ Html.BGColor (X11Color White)
                                                   , Html.Color (X11Color Black) -- the color used for all cellborders
                                                   , Html.Border 0  -- 0 = no border
                                                   , CellBorder 1
                                                   , CellSpacing 0
                                                   ]
                                , tableRows      = [ Cells -- Header row, containing the name of the class
                                                      [ LabelCell
                                                            [ Html.BGColor (X11Color Gray10)
                                                            , Html.Color   (X11Color Black)
                                                            ]
                                                            (Html.Text [ Html.Font [ Html.Color   (X11Color White)
                                                                                 ]
                                                                                 [Html.Str (fromString (name cl))]
                                                                      ]
                                                            )
                                                      ]
                                                   ]++
                                                   map attrib2row (clAtts cl) ++
                                                   map method2row (clMths cl)

                                }
                 where
                   attrib2row a = Cells
                                    [ Html.LabelCell [ Html.Align HLeft
                                                     , (Port .PN .fromString) (attNm a)
                                                     ]
                                         ( Html.Text [ Html.Str (fromString (if attOptional a then "o " else "+ "))
                                                     , Html.Str (fromString (name a))
                                                     , Html.Str (fromString " : ")
                                                     , Html.Str (fromString (attTyp a))
                                                     ]
                                         )
                                    ]
                   method2row m = Cells
                                    [ Html.LabelCell [ Html.Align HLeft]
                                         ( Html.Text [ Html.Str (fromString "+ ")
                                                     , Html.Str (fromString (show m))
                                                     ]
                                         )
                                    ]

          nonClass2node :: String -> DotNode String
          nonClass2node str = DotNode { nodeID = str
                                      , nodeAttributes = [ Shape Box3D
                                                         , Label (StrLabel (fromString str))
                                                         ]
                                      }

  -------------------------------
  --        ASSOCIATIONS:      --
  -------------------------------
          association2edge :: Association -> DotEdge String
          association2edge ass = trace ("Assoc: "++assSrc ass++" ---"++assrhr ass++"---> "++assTgt ass ++"    src port"++assSrcPort ass) $
             DotEdge { fromNode       = assSrc ass
                     , toNode         = assTgt ass
                     , edgeAttributes = [ ArrowHead (AType [(ArrMod OpenArrow BothSides, Normal)])  -- No arrowHead
                                        , ArrowTail (AType [(ArrMod OpenArrow BothSides, Normal)])  -- No arrowTail
                                        , HeadLabel (mult2Lable (assrhm ass))
                                        , TailLabel (mult2Lable (asslhm ass))
                                        , Label     (StrLabel (fromString (assrhr ass)))
                                        , LabelFloat True
                                        ]
                                      ++[(TailPort (LabelledPort (PN ((fromString.assSrcPort) ass)) Nothing))]
                     }
              where
                 mult2Lable = StrLabel . fromString . mult2Str
                 mult2Str (Mult MinZero MaxOne)  = "0-1"
                 mult2Str (Mult MinZero MaxMany) = "*"
                 mult2Str (Mult MinOne  MaxOne)  = "1"
                 mult2Str (Mult MinOne  MaxMany) = "1-*"

  -------------------------------
  --        AGGREGATIONS:      --
  -------------------------------
          aggregation2edge :: Aggregation -> DotEdge String
          aggregation2edge agg =
             DotEdge { fromNode       = (name.aggSrc) agg
                     , toNode         = (name.aggTgt) agg
                     , edgeAttributes = [ ArrowHead (AType [(ArrMod OpenArrow BothSides, NoArrow)])  -- No arrowHead
                                        , ArrowTail (AType [(ArrMod (case aggDel agg of
                                                                      Open -> OpenArrow
                                                                      Close -> FilledArrow
                                                                    ) BothSides , Diamond)
                                                           ])
                                        ]
                     }

  -------------------------------
  --        GENERALIZATIONS:   --       -- Ampersand statements such as "SPEC Dolphin ISA Animal" are called generalization.
  --                           --       -- Generalizations are represented by a red arrow with a (larger) open triangle as arrowhead
  -------------------------------
          generalization2edges :: Generalization -> [DotEdge String]
          generalization2edges ooGen = sub2edges (genAgen ooGen)
           where
             sub2edges gen
              = [DotEdge { fromNode = name spec
                         , toNode   = name gener
                         , edgeAttributes
                                    = [ ArrowTail (AType [(ArrMod OpenArrow BothSides, NoArrow)])  -- No arrowTail
                                      , ArrowHead (AType [(ArrMod OpenArrow BothSides, Normal)])   -- Open normal arrowHead
                                      , ArrowSize  2.0
                                      ] ++
                                      ( if blackWhite opts
                                        then [Style [SItem Dashed []]]
                                        else [GVcomp.Color [WC (X11Color Red) Nothing]]
                                      )
                         }
                | (spec,gener)<-splits gen]
             splits gen = case gen of
                                  Isa{} -> [(genspc gen, gengen gen)]
                                  IsE{} -> [(genspc gen, x ) | x<-(genrhs gen)]

-------------- Class Diagrams ------------------
   data ClassDiag = OOclassdiagram {cdName :: String
                                   ,classes :: [Class]           --
                                   ,assocs :: [Association]      --
                                   ,aggrs ::  [Aggregation]      --
                                   ,geners :: [Generalization]   --
                                   ,ooCpts :: [A_Concept]}
                            deriving Show
   instance Identified ClassDiag where
      name = cdName

   data Class          = OOClass  { clName :: String          -- ^ name of the class
                                  , clcpt ::  Maybe (A_Concept, [Purpose]) -- ^ Main concept of the class. (link tables do not have a main concept)
                                  , clAtts :: [CdAttribute]   -- ^ Attributes of the class
                                  , clMths :: [Method]        -- ^ Methods of the class
                                  } deriving Show
   instance Identified Class where
      name = clName
   data CdAttribute    = OOAttr   { attNm :: String            -- ^ name of the attribute
                                  , attTyp :: String           -- ^ type of the attribute (Concept name or built-in type)
                                  , attOptional :: Bool        -- ^ says whether the attribute is optional
                                  } deriving Show
   instance Identified CdAttribute where
      name = attNm
   data MinValue = MinZero | MinOne deriving (Show, Eq)

   data MaxValue = MaxOne | MaxMany deriving (Show, Eq)

   data Multiplicities = Mult MinValue MaxValue deriving Show

   data Association    = OOAssoc  { assSrc ::     String           -- ^ source: the name of the source class
                                  , assSrcPort :: String           -- ^ the name of the attribute in the source class
                                  , asslhm ::     Multiplicities   -- ^ left hand side multiplicities
                                  , asslhr ::     String           -- ^ left hand side role
                                  , assTgt ::     String           -- ^ target: the name of the target class
                                  , assrhm ::     Multiplicities   -- ^ right hand side multiplicities
                                  , assrhr ::     String           -- ^ right hand side role
                                  , asspurp ::    [Purpose]        -- ^ purposes of this association
                                  , assmean ::    Maybe A_Markup   -- ^ meaning, if available.
                                  } deriving Show
   data Aggregation    = OOAggr   { aggDel :: Deleting         --
                                  , aggSrc :: A_Concept           --
                                  , aggTgt :: A_Concept           --
                                  } deriving (Show, Eq)
   data Generalization = OOGener  { genAgen :: A_Gen             --
                                  } deriving (Show)

   data Deleting       = Open | Close                      --
                                    deriving (Show, Eq)
   data Method         = OOMethodC      String             -- name of this method, which creates a new object (producing a handle)
                                        [CdAttribute]      -- list of parameters: attribute names and types
                       | OOMethodR      String             -- name of this method, which yields the attribute values of an object (using a handle).
                                        [CdAttribute]      -- list of parameters: attribute names and types
                       | OOMethodS      String             -- name of this method, which selects an object using key attributes (producing a handle).
                                        [CdAttribute]      -- list of parameters: attribute names and types
                       | OOMethodU      String             -- name of this method, which updates an object (using a handle).
                                        [CdAttribute]      -- list of parameters: attribute names and types
                       | OOMethodD      String             -- name of this method, which deletes an object (using nothing but a handle).
                       | OOMethod       String             -- name of this method, which deletes an object (using nothing but a handle).
                                        [CdAttribute]      -- list of parameters: attribute names and types
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
