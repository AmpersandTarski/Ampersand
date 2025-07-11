{-# LANGUAGE FlexibleInstances #-}

module Ampersand.Graphic.ClassDiag2Dot
  ( classdiagram2dot,
  )
where

import Ampersand.ADL1 hiding (Box)
import Ampersand.Basics hiding (Label)
import Ampersand.Classes
import Ampersand.Graphic.ClassDiagram
import Ampersand.Misc.HasClasses
import Data.GraphViz.Attributes as GVatt
import Data.GraphViz.Attributes.Complete as GVcomp
import qualified Data.GraphViz.Attributes.HTML as Html
import Data.GraphViz.Types.Canonical hiding (attrs)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL

-- | translate a ClassDiagram to a DotGraph, so it can be used to show it as a picture.
classdiagram2dot :: (HasBlackWhite env) => env -> ClassDiag -> DotGraph MyDotNode
classdiagram2dot env cd =
  DotGraph
    { strictGraph = False,
      directedGraph = True,
      graphID = Nothing,
      graphStatements =
        DotStmts
          { attrStmts =
              GraphAttrs
                [ RankDir FromLeft,
                  bgColor White
                ]
                :
                --    ++ [NodeAttrs  [ ]]
                [ EdgeAttrs
                    [ FontSize 11,
                      MinLen 4
                    ]
                ],
            subGraphs =
              mapMaybe group2subgraph
                . filter (isJust . fst)
                . classesBySubgraph
                $ cd,
            nodeStmts =
              map class2node
                . concatMap (toList . snd)
                . filter (isNothing . fst)
                . classesBySubgraph
                $ cd,
            edgeStmts =
              map association2edge (assocs cd)
                ++ concatMap generalization2edges (geners cd)
          }
    }
  where
    group2subgraph :: (Maybe Name, NonEmpty Class) -> Maybe (DotSubGraph MyDotNode)
    group2subgraph (x, clss) = case x of
      Nothing -> Nothing
      Just nm ->
        Just
          DotSG
            { isCluster = True,
              subGraphID = Just . Str . TL.fromStrict . fullName $ nm,
              subGraphStmts =
                DotStmts
                  { attrStmts =
                      [ GraphAttrs
                          [ Label . StrLabel . TL.fromStrict . fullName $ nm,
                            -- URL "https://ampersandtarski.github.io/",
                            BgColor [WC (X11Color GhostWhite) Nothing]
                          ]
                      ],
                    subGraphs = [],
                    nodeStmts = class2node <$> toList clss,
                    edgeStmts = []
                  }
            }

    class2node :: Class -> DotNode MyDotNode
    class2node cl =
      DotNode
        { nodeID = toMyDotNode cl,
          nodeAttributes =
            [ Shape PlainText,
              GVcomp.Color [WC (X11Color Purple) Nothing],
              Label (HtmlLabel (Html.Table htmlTable))
            ]
        }
      where
        htmlTable =
          Html.HTable
            { Html.tableFontAttrs = Nothing,
              Html.tableAttrs =
                [ Html.BGColor (X11Color White),
                  Html.Color (X11Color Black), -- the color used for all cellborders
                  Html.Border 0, -- 0 = no border
                  Html.CellBorder 1,
                  Html.CellSpacing 0
                ],
              Html.tableRows = header : map attrib2row (clAtts cl)
            }
          where
            header =
              Html.Cells -- Header row, containing the name of the class
                [ Html.LabelCell
                    [ Html.BGColor (X11Color RoyalBlue),
                      Html.Color (X11Color Black)
                    ]
                    ( Html.Text
                        [ Html.Font
                            [Html.Color (X11Color Gray90)]
                            [Html.Str . fromString . T.unpack . fullName $ cl]
                        ]
                    )
                ]
            attrib2row a =
              Html.Cells
                [ Html.LabelCell
                    [ Html.Align Html.HLeft,
                      Html.Port . PN . fromString . T.unpack . fullName $ a
                    ]
                    ( Html.Text
                        [ Html.Str
                            ( fromString
                                ( case (Tot `elem` attProps a, Uni `elem` attProps a) of
                                    (True, True) -> "+"
                                    (True, False) -> "m+"
                                    (False, True) -> "o"
                                    (False, False) -> "m"
                                )
                                <> " "
                            ),
                          Html.Str . fromString . T.unpack . fullName $ a,
                          Html.Str (fromString " : "),
                          Html.Str . fromString . T.unpack . fullName . attTyp $ a
                        ]
                    )
                ]

    association2edge :: Association -> DotEdge MyDotNode
    association2edge ass =
      DotEdge
        { fromNode = toMyDotNode $ assSrc ass,
          toNode = toMyDotNode $ assTgt ass,
          edgeAttributes =
            [ ArrowHead (AType [(ArrMod OpenArrow BothSides, NoArrow)]), -- No arrowHead
              HeadLabel (mult2Lable (assrhm ass)),
              TailLabel (mult2Lable (asslhm ass)),
              Label . StrLabel . fromString . T.unpack . maybe mempty fullName . assrhr $ ass,
              LabelFloat True
            ]
              ++ [TailPort (LabelledPort (PN . fromString . T.unpack . fullName . assSrcPort $ ass) Nothing)]
        }
      where
        mult2Lable = StrLabel . fromString . mult2Str
        mult2Str (Mult MinZero MaxOne) = "0-1"
        mult2Str (Mult MinZero MaxMany) = "*"
        mult2Str (Mult MinOne MaxOne) = "1"
        mult2Str (Mult MinOne MaxMany) = "1-*"

    -------------------------------
    --        GENERALIZATIONS:   --       -- Ampersand statements such as "CLASSIFY Dolphin ISA Animal" are called generalization.
    --                           --       -- Generalizations are represented by a red arrow with a (larger) open triangle as arrowhead
    -------------------------------
    generalization2edges :: Generalization -> [DotEdge MyDotNode]
    generalization2edges ooGen = sub2edges (genAgen ooGen)
      where
        -- SJ 2017-09-22. Drawing generalization arrows from right to left seems to give better pictures.
        -- This is probably because it is more in line with the totality constraint, which governs the drawing
        -- direction of associations. For this purpose we draw a backward facing arrow from generic to specific.

        sub2edges :: AClassify -> [DotEdge MyDotNode]
        sub2edges gen =
          [ DotEdge
              { fromNode = toMyDotNode gener,
                toNode = toMyDotNode spec,
                edgeAttributes =
                  [ Dir Back,
                    ArrowTail (AType [(ArrMod OpenArrow BothSides, Normal)]), -- Open normal arrowHead
                    ArrowSize 2.0
                  ]
                    ++ ( if view blackWhiteL env
                           then [GVcomp.Style [SItem Dashed []]]
                           else [GVcomp.Color [WC (X11Color Red) Nothing]]
                       )
              }
            | (spec, gener) <- splits gen,
              spec /= gener -- required, until issue #896 is fixed.
          ]
        splits gen = case gen of
          Isa {} -> [(genspc gen, gengen gen)]
          IsE {} -> [(genspc gen, x) | x <- NE.toList $ genrhs gen]

classesBySubgraph :: ClassDiag -> [(Maybe Name, NonEmpty Class)]
classesBySubgraph = map foo . eqCl snd . classes
  where
    foo x = (snd . NE.head $ x, fst <$> x)

classesOfSubgraphs :: ClassDiag -> [Class]
classesOfSubgraphs =
  map fst
    . filter (isJust . snd)
    -- . groupsAndRest
    . classes

class CdNode a where
  nodes :: a -> [Name]

instance CdNode ClassDiag where
  nodes cd =
    L.nub
      ( concat
          ( mempty -- map nodes (classes cd)
              ++ map nodes (classesOfSubgraphs cd)
              ++ map nodes (assocs cd)
              ++ map nodes (geners cd)
          )
      )

instance CdNode (Name, NonEmpty Class) where
  nodes = nodes . toList . snd

instance CdNode Class where
  nodes cl = [name . clName $ cl]

instance CdNode (Class, Maybe Name) where
  nodes = nodes . fst

instance (CdNode a) => CdNode [a] where
  nodes = concatMap nodes

instance CdNode Association where
  nodes a = [name . assSrc $ a, name . assTgt $ a]

instance CdNode Aggregation where
  nodes (OOAggr _ s t) = map name [s, t]

instance CdNode Generalization where
  nodes = map name . toList . concs . genAgen
