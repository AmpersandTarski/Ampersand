module Database.Design.Ampersand.Graphic.ClassDiag2Dot ( 
  classdiagram2dot

) 
where
import Data.List
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Classes
import Database.Design.Ampersand.ADL1  hiding (Association,Box)
import Database.Design.Ampersand.Misc
import Data.String
import Data.GraphViz.Types.Canonical hiding (attrs)
import Data.GraphViz.Attributes.Complete as GVcomp
import Data.GraphViz.Attributes as GVatt
import Data.GraphViz.Attributes.HTML as Html
import Database.Design.Ampersand.Graphic.ClassDiagram

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

---- In order to make classes, all relations that are univalent and injective are flipped
---- attRels contains all relations that occur as attributes in classes.
--       attRels    = [r |r<-rels, isUni r, not (isInj r)]        ++[flp r |r<-rels, not (isUni r), isInj r] ++
--                    [r |r<-rels, isUni r,      isInj r, isSur r]++[flp r |r<-rels,      isUni r , isInj r, not (isSur r)]
---- assRels contains all relations that do not occur as attributes in classes
--       assRels    = [r |r<-relsLim, not (isUni r), not (isInj r)]
--       attrs rs   = [ OOAttr ((name.head.relsMentionedIn) r) (name (target r)) (not(isTot r))
--                    | r<-rs, not (isPropty r)]
--       isPropty r = null([Sym,Asy]>-properties r)

-------------------------------
--        ASSOCIATIONS:      --
-------------------------------
       association2edge :: Association -> DotEdge String
       association2edge ass =
          DotEdge { fromNode       = assSrc ass
                  , toNode         = assTgt ass
                  , edgeAttributes = [ ArrowHead (AType [(ArrMod OpenArrow BothSides, NoArrow)])  -- No arrowHead
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
          DotEdge { fromNode       = name . aggChild  $ agg
                  , toNode         = name . aggParent $ agg
                  , edgeAttributes = [ ArrowHead (AType [(ArrMod (case aggDel agg of
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
                                 = [ ArrowHead (AType [(ArrMod OpenArrow BothSides, Normal)])   -- Open normal arrowHead
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
instance CdNode Association where
 nodes a = [assSrc a,assTgt a]

instance CdNode Aggregation where
 nodes (OOAggr _ s t) = map name [s,t]

instance CdNode Generalization where
 nodes g = map name ((concs.genAgen) g)

