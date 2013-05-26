{-# OPTIONS_GHC -Wall #-}
--TODO -> clean and stuff. Among which moving classdiagram2dot to Graphviz library implementation (see Classes/Graphics.hs).
--        I only helped it on its feet and I have put in the fSpec, now it generates stuff. I like stuff :)

module DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram
         (ClassDiag(..), Class(..), CdAttribute(..), Association(..), Aggregation(..), Generalization(..), Deleting(..), Method(..),
          Multiplicities(..), MinValue(..), MaxValue(..),
          clAnalysis, cdAnalysis, tdAnalysis, classdiagram2dot)
where
   import Data.List
   import DatabaseDesign.Ampersand.Basics
   import DatabaseDesign.Ampersand.Classes
   import DatabaseDesign.Ampersand.ADL1  hiding (Association,Box)
   import DatabaseDesign.Ampersand.Fspec.Plug
   import DatabaseDesign.Ampersand.Misc
   import DatabaseDesign.Ampersand.Fspec.Fspec
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
    nodes OOClass{clName=nm} = [nm]
   instance CdNode a => CdNode [a] where
    nodes = concatMap nodes

   instance CdNode CdAttribute where
    nodes (OOAttr _ t _) = [t]

   instance CdNode Method where
    nodes _ = []

   instance CdNode Association where
    nodes (OOAssoc s _ _ t _ _) = map nm [s,t]
      where
       nm (Left  c  ) = name c
       nm (Right str) = str

   instance CdNode Aggregation where
    nodes (OOAggr _ s t) = map name [s,t]

   instance CdNode Generalization where
    nodes (OOGener g ss) = map name (g:ss)

   -- | This function makes the classification diagram.
   -- It focuses on generalizations and specializations.
   clAnalysis :: Fspc -> Options -> ClassDiag
   clAnalysis fSpec _ = 
       OOclassdiagram { cdName  = "classification_"++name fSpec
                      , classes = [ OOClass { clName = name c
                                            , clcpt  = Just c
                                            , clAtts = attrs c
                                            , clMths = [] 
                                            } | c<-cpts]
                      , assocs  = []
                      , aggrs   = []
                      , geners  = nub [ OOGener c (map snd gs)
                                      | gs<-eqCl fst (fsisa fSpec)
                                      , let c=fst (head gs), c `elem` cpts -- select only those generalisations whose specific concept is part of the themes to be printed.
                        ]
                      , ooCpts  = concs fSpec
                      }

    where
       rels       = [EDcD r (sign r) | r@Sgn{} <- declarations fSpec, decusr r]
       relsAtts   = [r | e<-rels, r<-[e, flp e], isUni r]
       cpts       = nub [ specific
                        | specific <-map fst (fsisa fSpec)
                         -- select only those generalisations whose specific concept is part of the themes to be printed.
                        , specific `elem` (concs [declsUsedIn (pattsInScope fSpec) 
                                            `uni` declsUsedIn (procsInScope fSpec)
                                                 ])
                        , (not.null) [ r | r<-relsAtts, source r==specific ] ||  -- c is either a concept that has attributes or
                               null  [ r | r<-relsAtts, target r==specific ]     --      it does not occur as an attribute.
                        ]

       attrs c    = [ OOAttr (fldname fld) (if isPropty fld then "Bool" else  name (target (fldexpr fld))) (fldnull fld)
                    | plug<-lookup' c, fld<-tail (plugFields plug), not (inKernel fld), source (fldexpr fld)==c]
                    where inKernel fld = null([Uni,Inj,Sur]>-multiplicities (fldexpr fld)) && not (isPropty fld)
       lookup' c = [plug |InternalPlug plug@TblSQL{}<-plugInfos fSpec , (c',_)<-cLkpTbl plug, c'==c]
       isPropty fld = null([Sym,Asy]>-multiplicities (fldexpr fld))
        
   pattsInScope :: Fspc -> [Pattern]
   pattsInScope fSpec = if null (themes fSpec)
                        then patterns fSpec
                        else [ pat | pat<-patterns fSpec, name pat `elem` themes fSpec ]
   procsInScope :: Fspc -> [Process]
   procsInScope fSpec = if null (themes fSpec)
                        then map fpProc (vprocesses fSpec)
                        else [ fpProc fprc | fprc<-vprocesses fSpec, name fprc `elem` themes fSpec ]



   -- | This function, cdAnalysis, generates a conceptual data model.
   -- It creates a class diagram in which generalizations and specializations remain distinct entity types.
   -- This yields more classes than plugs2classdiagram does, as plugs contain their specialized concepts.
   -- Properties and identities are not shown.
   cdAnalysis :: Fspc -> Options -> ClassDiag
   cdAnalysis fSpec _ = 
     OOclassdiagram {cdName  = "logical_"++name fSpec
                    ,classes = 
                       [ OOClass{ clName = name root
                                , clcpt  = Just root
                                , clAtts = map ooAttr (filter (\x -> source x == root) attribs)
                                , clMths = []
                                }
                       | root <- roots]
                    ,assocs  = 
                       [ OOAssoc { assSrc = Left (source r)
                                 , asslhm = (mults.flp) r
                                 , asslhr = ""
                                 , assTrg = Left (target r)
                                 , assrhm = mults r
                                 , assrhr = (name.head.declsUsedIn) r
                                 }
                       | r <- allrels, target r `elem` roots
                       ]
                    ,aggrs   = []
                    ,geners  = []
                    ,ooCpts  = roots
                    }

    where
      ooAttr r = OOAttr { attNm = (name.head.declsUsedIn) r
                        , attTyp = (name.target) r
                        , attOptional = (not.isTot) r
                        }
      mults r = let minVal = if isTot r then MinOne else MinZero
                    maxVal = if isInj r then MaxOne else MaxMany
                in  Mult minVal maxVal 
      allrels = [ EDcD r (sign r) -- restricted to those themes that must be printed.
                | r <- (nub.concat)
                       ([declarations p ++ declsUsedIn p  | p <- pattsInScope fSpec ]++
                        [declarations p ++ declsUsedIn p  | p <- procsInScope fSpec ])
               , decusr r]
      attribs = map flipWhenInj (filter isAttribRel allrels)
          where isAttribRel r = isUni r || isInj r
                flipWhenInj r = if isInj r then flp r else r
      roots = nub (map source allrels)
                     
   -- | This function generates a technical data model.
   -- It is based on the plugs that are calculated.
   tdAnalysis :: Fspc -> Options -> ClassDiag
   tdAnalysis fSpec _ = 
     OOclassdiagram {cdName  = "technical_"++name fSpec
                    ,classes = 
                       [ OOClass{ clName = sqlname table
                                , clcpt  = primKey table
                                , clAtts = case table of
                                              TblSQL{fields=(_:attribs)} -> map (ooAttr.fldexpr) attribs
                                              BinSQL{columns=(a,b)}      -> 
                                                 [OOAttr { attNm       = fldname a
                                                         , attTyp      = (name.target.fldexpr) a
                                                         , attOptional = False
                                                         }
                                                 ,OOAttr { attNm       = fldname b
                                                         , attTyp      = (name.target.fldexpr) b
                                                         , attOptional = False
                                                         }
                                                 ]
                                              _     -> fatal 166 "Unexpeced type of table!"
                                , clMths = []
                                }
                       | table <- tables]
                    ,assocs  = allAssocs
                    ,aggrs   = []
                    ,geners  = []
                    ,ooCpts  = roots
                    }
     where
      tables = [ pSql | InternalPlug pSql <- plugInfos fSpec, not (isScalar pSql)]
         where isScalar ScalarSQL{} = True
               isScalar _           = False 
      roots :: [A_Concept]
      roots = catMaybes (map primKey tables)
      primKey TblSQL{fields=(f:_)} = Just (source (fldexpr f))
      primKey _                    = Nothing
      ooAttr :: Expression -> CdAttribute
      ooAttr r = OOAttr { attNm = (name.head.declsUsedIn) r
                        , attTyp = (name.target) r
                        , attOptional = (not.isTot) r
                        }
      allAssocs = concatMap relsOf tables
        where
          relsOf t = 
            case t of
              TblSQL{} -> map mkRel (catMaybes [relOf fld | fld <- fields t])
              BinSQL{columns=(a,b)} -> 
                        [ OOAssoc { assSrc = Right (sqlname t)
                                  , asslhm = Mult MinZero MaxMany
                                  , asslhr = ""
                                  , assTrg = (Left .target.fldexpr) a
                                  , assrhm = Mult MinOne MaxOne
                                  , assrhr = ""
                                  }
                        , OOAssoc { assSrc = Right (sqlname t)
                                  , asslhm = Mult MinZero MaxMany
                                  , asslhr = ""
                                  , assTrg = (Left .target.fldexpr) b
                                  , assrhm = Mult MinOne MaxOne
                                  , assrhr = ""
                                  }
                        ]
              _  -> fatal 195 "Unexpeced type of table!"
          relOf f = 
            let expr = fldexpr f in
            case expr of
              EDcI{} -> Nothing
              ETyp EDcI{} _ -> Nothing    
              EDcD _ sgn -> if target sgn `elem` roots then Just expr else Nothing
              _ -> fatal 200 ("Unexpected expression: "++show expr)
          mkRel :: Expression -> Association
          mkRel r = OOAssoc { assSrc = Left (source r)
                            , asslhm = (mults.flp) r
                            , asslhr = ""
                            , assTrg = Left (target r)
                            , assrhm = mults r
                            , assrhr = (name.head.declsUsedIn) r
                            }
      mults r = let minVal = if isTot r then MinOne else MinZero
                    maxVal = if isInj r then MaxOne else MaxMany
                in  Mult minVal maxVal 

   mshow [] = "."
   mshow (e:es) = "\n"++myshowExpr e++mshow es    
   myshowExpr expr = 
      case expr of
        EDcD r _   -> show(name r)++show (sign r)
        EFlp e _    -> "flp("++myshowExpr e++")"
        _ -> fatal 154 (show expr)

---- In order to make classes, all relations that are univalent and injective are flipped
---- attRels contains all relations that occur as attributes in classes.
--       attRels    = [r |r<-rels, isUni r, not (isInj r)]        ++[flp r |r<-rels, not (isUni r), isInj r] ++
--                    [r |r<-rels, isUni r,      isInj r, isSur r]++[flp r |r<-rels,      isUni r , isInj r, not (isSur r)]
---- assRels contains all relations that do not occur as attributes in classes
--       assRels    = [r |r<-relsLim, not (isUni r), not (isInj r)]
--       attrs rs   = [ OOAttr ((name.head.declsUsedIn) r) (name (target r)) (not(isTot r))
--                    | r<-rs, not (isPropty r)]
--       isPropty r = null([Sym,Asy]>-multiplicities r)
       
   -- | translate a ClassDiagram to a DotGraph, so it can be used to show it as a picture. 
   classdiagram2dot :: Options -> ClassDiag -> DotGraph String
   classdiagram2dot flags cd
    = DotGraph { strictGraph   = False
               , directedGraph = True
               , graphID       = Nothing
               , graphStatements = dotstmts
               }
        where
         dotstmts = DotStmts
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
                                    [ Html.LabelCell [ Html.Align HLeft] 
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
          association2edge ass = 
             DotEdge { fromNode       = case assSrc ass of
                                          Left c  -> name c
                                          Right s -> s
                     , toNode         = case assTrg ass of
                                          Left c  -> name c
                                          Right s -> s
                     , edgeAttributes = [ ArrowHead (AType [(ArrMod OpenArrow BothSides, NoArrow)])  -- No arrowHead
                                        , ArrowTail (AType [(ArrMod OpenArrow BothSides, NoArrow)])  -- No arrowTail
                                        , HeadLabel (mult2Lable (assrhm ass))
                                        , TailLabel (mult2Lable (asslhm ass))
                                        , Label     (StrLabel (fromString (assrhr ass)))
                                        , LabelFloat True
                                        ]
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
                     , toNode         = (name.aggTrg) agg
                     , edgeAttributes = [ ArrowHead (AType [(ArrMod OpenArrow BothSides, NoArrow)])  -- No arrowHead
                                        , ArrowTail (AType [(ArrMod (case aggDel agg of 
                                                                      Open -> OpenArrow
                                                                      Close -> FilledArrow
                                                                    ) BothSides , Diamond)
                                                           ]) 
                                        ]
                     }
                 
 
 
  -------------------------------
  --        GENERALIZATIONS:   --       -- Ampersand statements such as "GEN Dolphin ISA Animal" are called generalization.
  --                           --       -- Generalizations are represented by a red arrow with a (larger) open triangle as arrowhead 
  -------------------------------
          generalization2edges :: Generalization -> [DotEdge String]
          generalization2edges ooGen = map (sub2edge (genGen ooGen)) (genSubs ooGen)
           where
             sub2edge g s = DotEdge
                              { fromNode = name g
                              , toNode   = name s
                              , edgeAttributes 
                                         = [ ArrowTail (AType [(ArrMod OpenArrow BothSides, NoArrow)])  -- No arrowTail
                                           , ArrowHead (AType [(ArrMod OpenArrow BothSides, Normal)])   -- Open normal arrowHead
                                           , ArrowSize  2.0
                                           ] ++
                                           ( if blackWhite flags
                                             then [Style [SItem Dashed []]]
                                             else [GVcomp.Color [WC (X11Color Red) Nothing]]
                                           )
                              }
             



-------------- Class Diagrams ------------------
   data ClassDiag = OOclassdiagram {cdName :: String
                                   ,classes :: [Class]           --
                                   ,assocs :: [Association]      --
                                   ,aggrs  :: [Aggregation]      --
                                   ,geners :: [Generalization]   --
                                   ,ooCpts :: [A_Concept]}
                            deriving Show
   instance Identified ClassDiag where
      name = cdName
        
   data Class          = OOClass  { clName :: String          -- ^ name of the class
                                  , clcpt  :: Maybe A_Concept -- ^ Main concept of the class. (link tables do not have a main concept)
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
   
   data Association    = OOAssoc  { assSrc :: Either A_Concept String -- ^ source: the left hand side class. In case of a link table, the name of that table.
                                  , asslhm :: Multiplicities   -- ^ left hand side multiplicities
                                  , asslhr :: String           -- ^ left hand side role
                                  , assTrg :: Either A_Concept String -- ^ target: the right hand side class. In case of a link table, the name of that table.
                                  , assrhm :: Multiplicities   -- ^ right hand side multiplicities
                                  , assrhr :: String           -- ^ right hand side role
                                  }  deriving Show
   data Aggregation    = OOAggr   { aggDel :: Deleting         -- 
                                  , aggSrc :: A_Concept           --
                                  , aggTrg :: A_Concept           --
                                  } deriving (Show, Eq)
   data Generalization = OOGener  { genGen :: A_Concept             --
                                  , genSubs:: [A_Concept]           --
                                  } deriving (Show, Eq)



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
