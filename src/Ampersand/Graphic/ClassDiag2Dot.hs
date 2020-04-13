module Ampersand.Graphic.ClassDiag2Dot ( 
  classdiagram2dot

) 
where
import           Ampersand.ADL1  hiding (Box)
import           Ampersand.Basics
import           Ampersand.Classes
import           Ampersand.Graphic.ClassDiagram
import           Ampersand.Misc.HasClasses
import           Data.GraphViz.Attributes as GVatt
import           Data.GraphViz.Attributes.Complete as GVcomp
import qualified Data.GraphViz.Attributes.HTML as Html
import           Data.GraphViz.Types.Canonical hiding (attrs)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T

-- | translate a ClassDiagram to a DotGraph, so it can be used to show it as a picture.
classdiagram2dot :: (HasBlackWhite env) => env -> ClassDiag -> DotGraph Text
classdiagram2dot env cd
 = DotGraph { strictGraph     = False
            , directedGraph   = True
            , graphID         = Nothing
            , graphStatements = 
                    DotStmts
                        { attrStmts =  GraphAttrs [ RankDir FromLeft
                                                  , bgColor White]
                                       :
                                --    ++ [NodeAttrs  [ ]]
                                       [EdgeAttrs  [ FontSize 11
                                                   , MinLen 4
                                       ]           ]
                        , subGraphs = []
                        , nodeStmts = allNodes (classes cd) [n | n<- nodes cd
                                                            , n `notElem` nodes (classes cd)
                                                            ]
                        , edgeStmts = map association2edge (assocs cd)  ++
                                      map aggregation2edge (aggrs cd)  ++
                                      concatMap generalization2edges (geners cd)
                        }
            }
     where
       allNodes :: [Class] -> [Text] -> [DotNode Text]
       allNodes cs others =
          map class2node cs ++
          map nonClass2node others

       class2node :: Class -> DotNode Text
       class2node cl = DotNode
         { nodeID         = name cl
         , nodeAttributes = [ Shape PlainText
                            , GVcomp.Color [WC (X11Color Purple) Nothing]
                            , Label (HtmlLabel (Html.Table htmlTable))
                            ]
         } where
          htmlTable = Html.HTable
             { Html.tableFontAttrs = Nothing
             , Html.tableAttrs = [ Html.BGColor (X11Color White)
                                , Html.Color (X11Color Black) -- the color used for all cellborders
                                , Html.Border 0  -- 0 = no border
                                , Html.CellBorder 1
                                , Html.CellSpacing 0
                                ]
             , Html.tableRows = [ Html.Cells -- Header row, containing the name of the class
                                   [ Html.LabelCell
                                         [ Html.BGColor (X11Color Gray10)
                                         , Html.Color   (X11Color Black)
                                         ]
                                         (Html.Text [ Html.Font [ Html.Color   (X11Color White)
                                                              ]
                                                              [Html.Str (fromString (T.unpack $ name cl))]
                                                   ]
                                         )
                                   ]
                                ]++
                                map attrib2row (clAtts cl) ++
                                map method2row (clMths cl)
             }
              where
                attrib2row a = Html.Cells
                                 [ Html.LabelCell [ Html.Align Html.HLeft
                                                  , (Html.Port .PN .fromString) (T.unpack $ attNm a)
                                                  ]
                                      ( Html.Text [ Html.Str (fromString (if attOptional a then "o " else "+ "))
                                                  , Html.Str (fromString (T.unpack $ name a))
                                                  , Html.Str (fromString " : ")
                                                  , Html.Str (fromString (T.unpack $ attTyp a))
                                                  ]
                                      )
                                 ]
                method2row m = Html.Cells
                                 [ Html.LabelCell [ Html.Align Html.HLeft]
                                      ( Html.Text [ Html.Str (fromString "+ ")
                                                  , Html.Str (fromString (show m))
                                                  ]
                                      )
                                 ]

       nonClass2node :: Text -> DotNode Text
       nonClass2node str = DotNode { nodeID = str
                                   , nodeAttributes = [ Shape Box3D
                                                      , Label (StrLabel (fromString . T.unpack $ str))
                                                      ]
                                   }

---- In order to make classes, all relations that are univalent and injective are flipped
---- attRels contains all relations that occur as attributes in classes.
--       attRels    = [r |r<-rels, isUni r, not (isInj r)]        ++[flp r |r<-rels, not (isUni r), isInj r] ++
--                    [r |r<-rels, isUni r,      isInj r, isSur r]++[flp r |r<-rels,      isUni r , isInj r, not (isSur r)]
---- assRels contains all relations that do not occur as attributes in classes
--       assRels    = [r |r<-relsLim, not (isUni r), not (isInj r)]
--       attrs rs   = [ OOAttr ((name.head.bindedRelationsIn) r) (name (target r)) (not(isTot r))
--                    | r<-rs, not (isPropty r)]
--       isPropty r = null([Sym,Asy]Set.\\properties r)

-------------------------------
--        ASSOCIATIONS:      --
-------------------------------
       association2edge :: Association -> DotEdge Text
       association2edge ass =
          DotEdge { fromNode       = assSrc ass
                  , toNode         = assTgt ass
                  , edgeAttributes = [ ArrowHead (AType [(ArrMod OpenArrow BothSides, NoArrow)])  -- No arrowHead
                                     , HeadLabel (mult2Lable (assrhm ass))
                                     , TailLabel (mult2Lable (asslhm ass))
                                     , Label     (StrLabel (fromString (T.unpack $ assrhr ass)))
                                     , LabelFloat True
                                     ]
                                   ++[ TailPort (LabelledPort (PN ((fromString . T.unpack . assSrcPort) ass)) Nothing)]
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
       aggregation2edge :: Aggregation -> DotEdge Text
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
--        GENERALIZATIONS:   --       -- Ampersand statements such as "CLASSIFY Dolphin ISA Animal" are called generalization.
--                           --       -- Generalizations are represented by a red arrow with a (larger) open triangle as arrowhead
-------------------------------
       generalization2edges :: Generalization -> [DotEdge Text]
       generalization2edges ooGen = sub2edges (genAgen ooGen)
-- SJ 2017-09-22. Drawing generalization arrows from right to left seems to give better pictures.
-- This is probably because it is more in line with the totality constraint, which governs the drawing
-- direction of associations. For this purpose we draw a backward facing arrow from generic to specific.
        where
          sub2edges gen
           = [DotEdge { fromNode = name gener
                      , toNode   = name spec
                      , edgeAttributes
                                 = [ Dir Back
                                   , ArrowTail (AType [(ArrMod OpenArrow BothSides, Normal)])   -- Open normal arrowHead
                                   , ArrowSize  2.0
                                   ] ++
                                   ( if view blackWhiteL env
                                     then [GVcomp.Style [SItem Dashed []]]
                                     else [GVcomp.Color [WC (X11Color Red) Nothing]]
                                   )
                      }
             | (spec,gener)<-splits gen
             , spec /= gener -- required, until issue #896 is fixed.
             ] 
          splits gen = case gen of
                               Isa{} -> [(genspc gen, gengen gen)]
                               IsE{} -> [(genspc gen, x ) | x<-NE.toList $ genrhs gen]



class CdNode a where
 nodes :: a->[Text]

instance CdNode ClassDiag where
 nodes cd = L.nub (concat (  map nodes (classes cd)
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
 nodes = map name . Set.elems . concs . genAgen

