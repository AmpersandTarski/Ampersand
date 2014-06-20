{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.Switchboard
    (SwitchBdDiagram(..),switchboardAct,sbDiagram,processModel) where
 
   import Data.GraphViz
   import Data.GraphViz.Attributes.Complete
   import Data.List
   import DatabaseDesign.Ampersand.Basics        (fatalMsg,Identified(..), flp)
   import DatabaseDesign.Ampersand.ADL1
   import DatabaseDesign.Ampersand.Classes
   import DatabaseDesign.Ampersand.Fspec.Fspec
   import DatabaseDesign.Ampersand.Fspec.ShowADL (ShowADL(..), LanguageDependent(..))
   import Data.String
   
--   import DatabaseDesign.Ampersand.Fspec.ShowECA (showECA) -- for testing purposes
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.Switchboard"

   data SwitchBdDiagram
    = SBdgrm { sbName :: String
             , sbdotGraph :: DotGraph String
             }
   instance Identified SwitchBdDiagram where
      name = sbName

   processModel :: FProcess -> DotGraph String
   processModel fp
    = DotGraph { strictGraph = False
               , directedGraph = True
               , graphID = Just (Str (fromString "Process Model"))
               , graphStatements 
                  = DotStmts { attrStmts = [GraphAttrs [{-Splines SplineEdges,-} RankDir FromLeft]]
                             , subGraphs = []
                             , nodeStmts = activityNodes
                             , edgeStmts = edges
                             }
               }
      where
         activityNodes = [ DotNode { nodeID         = "act_"++name a
                                   , nodeAttributes = [Style [SItem Filled []], FillColor [WC(X11Color Orange) Nothing], Label (StrLabel (fromString (name a)))]
                                   }
                         | a<-fpActivities fp]
         edges         = nub
                         [ DotEdge { fromNode = "act_"++name from
                                   , toNode   = "act_"++name to
                                   , edgeAttributes = [Len 2, Label (StrLabel (fromString (show e++" "++name d))), Dir Forward]
                                   }
                         | (from,to,e,d) <- allEdges
                         ]
         allEdges  = nub[(from,to,e,d) | (e,d,from)<-eventsOut, (e',d',to)<-eventsIn, e==e', d==d']
{-
data Activity = Act { actRule ::   Rule
                    , actTrig ::   [Relation]
                    , actAffect :: [Relation]
                    , actQuads ::  [Quad]
                    , actEcas ::   [ECArule]
                    , actPurp ::   [Purpose]
                    }
data ECArule= ECA { ecaTriggr :: Event     -- The event on which this rule is activated
                  , ecaDelta :: Relation  -- The delta to be inserted or deleted from this rule. It actually serves very much like a formal parameter.
                  , ecaAction :: PAclause  -- The action to be taken when triggered.
                  , ecaNum :: Int       -- A unique number that identifies the ECArule within its scope.
                  }
data Event = On { eSrt :: InsDel
                  , eRel :: Relation
                  } deriving (Show,Eq)
-}
         eventsIn  = [(e,d,act) | act<-fpActivities fp, eca<-actEcas act, let On e d = ecaTriggr eca]
         eventsOut = [(e,d,act) | act<-fpActivities fp, eca<-actEcas act, (e,d)<-(nub.evs.ecaAction) eca]
                     where evs :: PAclause -> [(InsDel,Declaration)]
                           evs clause
                             = case clause of
                                CHC{} -> (concat.map evs) (paCls clause)
                                GCH{} -> concat [ evs p | (_,_,p)<-paGCls clause]
                                ALL{} -> (concat.map evs) (paCls clause)
                                Do{}  -> [(paSrt clause, paTo clause)]
                                New{} -> evs (paCl clause "")
                                Rmv{} -> evs (paCl clause "")
                                Nop{} -> []
                                Blk{} -> []
                                Let{} -> fatal 305 "events for let undetermined"
                                Ref{} -> fatal 306 "events for Ref undetermined"

   colorRule :: Rule -> X11Color
   colorRule r  | isSignal r = Orange
                | otherwise  = Green

   sbDiagram :: Fspc -> SwitchBdDiagram
   sbDiagram fSpec 
    = SBdgrm
        { sbName = name fSpec
        , sbdotGraph
           = DotGraph { strictGraph = False
                      , directedGraph = True
                      , graphID = Just (Str (fromString "Switchboard"))
                      , graphStatements 
                         = DotStmts { attrStmts = [GraphAttrs [Splines SplineEdges, RankDir FromLeft]]
                                    , subGraphs = []
                                    , nodeStmts = inEvNodes++conjNodes++ecaNodes++outEvNods
                                    , edgeStmts = edgesEvCj++edgesCjEc++edgesEcEv
                                    }
                      }
        }
      where
        fsb = fSwitchboard fSpec
        --DESCR -> The relations from which changes can come
        inEvNodes = [ DotNode { nodeID         = nameINode eventsIn ev
                              , nodeAttributes = [Label (StrLabel (fromString (show (eSrt ev)++" "++showADL (eDcl ev))))]
                              }
                    | ev<-eventsIn
                    ]
        --DESCR -> All conjuncts
        conjNodes = [ DotNode { nodeID         = nameCNode (fsbConjs fsb) (rul,c)
                              , nodeAttributes = [Style [SItem Filled []], FillColor [WC(X11Color (colorRule rul)) Nothing], (Label . StrLabel . fromString . name) rul]
                              }
                    | (rul,c)<-fsbConjs fsb]

        --DESCR -> All ECA rules
        ecaNodes  = [ DotNode { nodeID         = nameENode (fsbECAs fsb) eca
                              , nodeAttributes = if isBlk (ecaAction eca)
                                                 then [Style [SItem Filled []], FillColor [WC(X11Color Red)Nothing], (Label . StrLabel . fromString) ("ERR #"++show (ecaNum eca))]
                                                 else [(Label . StrLabel . fromString. showADL) eca]
                              }
                    | eca<-fsbECAs fsb, not (isBlk (ecaAction eca))]

        --DESCR -> The relations to which changes are made
        outEvNods = [ DotNode { nodeID         = nameONode eventsOut ev
                              , nodeAttributes = [Label (StrLabel (fromString (show (eSrt ev)++" "++showADL (eDcl ev))))]
                              }
                    | ev<-eventsOut
                    ]
        --DESCR -> Each edge represents an insert between a relation on the left and a term on the right to which the relation contributes to an insert.
        edgesEvCj = [ DotEdge { fromNode = nameINode eventsIn ev
                              , toNode   = nameCNode (fsbConjs fsb) (rul,c)
                              , edgeAttributes = [Dir Forward]
                              }
                    | (rul,c)<-fsbConjs fsb, ev<-eventsIn, eDcl ev `elem` relsUsedIn c]
        edgesCjEc = [ DotEdge { fromNode = nameCNode (fsbConjs fsb) (rul,c)
                              , toNode   = nameENode (fsbECAs fsb) eca
                              , edgeAttributes = [Dir Forward]
                              }
                    | (rul,c)<-fsbConjs fsb, eca<-fsbECAs fsb, not (isBlk (ecaAction eca)), rul `elem` concat [r | (_,r)<-paMotiv (ecaAction eca)] ]
        edgesEcEv = nub
                    [ DotEdge { fromNode = nameENode (fsbECAs fsb) eca
                              , toNode   = nameONode eventsOut (On tOp rel)
                              , edgeAttributes = [Dir Forward]
                              }
                    | eca<-fsbECAs fsb, not (isBlk (ecaAction eca))
                    , On tOp rel<-eventsFrom (ecaAction eca)
                    ]
        nameINode = nmLkp fSpec "in_"
        nameCNode = nmLkp fSpec "cj_"
        nameENode = nmLkp fSpec "eca_"
        nameONode = nmLkp fSpec "out_"
        eventsIn  = nub [ecaTriggr eca | eca<-fsbECAs fsb, not (isBlk (ecaAction eca)) ]
        eventsOut = nub [evt | eca<-fsbECAs fsb, let act=ecaAction eca, not (isBlk act), evt<-eventsFrom act]

   switchboardAct :: Fspc -> Activity -> SwitchBdDiagram
   switchboardAct fSpec act
    = SBdgrm
        { sbName = name act
        , sbdotGraph
           = DotGraph { strictGraph = False
                      , directedGraph = True
                      , graphID = Just (Str (fromString "Switchboard"))
                      , graphStatements 
                         = DotStmts { attrStmts = [GraphAttrs [Splines SplineEdges, RankDir FromLeft]]
                                    , subGraphs = []
                                    , nodeStmts = inMorNodes++conjunctNodes++outMorNodes
                                    , edgeStmts = edgesIn++edgesOut
                                    }
                      }
        }
      where
        fromRels     = nub (actTrig act)
        toRels :: [Declaration]
        toRels       = nub (actAffect act)
        conjuncts    = nub [(cl_rule ccrs,c)
                           | Quad _ ccrs<-actQuads act, c<- map rc_conjunct (cl_conjNF ccrs)]
        --DESCR -> The relations from which changes can come
        inMorNodes    = [ DotNode { nodeID         = nameINode fromRels r
                                  , nodeAttributes = [Label (StrLabel (fromString (showADL r)))]
                                  }
                        | r<-fromRels
                                                  --TODOHAN        , (not.null) [e |e<-edgesIn, (nodeID  (fromNode e))==nameINode fromRels r]
                        ]

        --DESCR -> The relations to which changes are made
        outMorNodes   = [ DotNode { nodeID         = nameONode toRels r
                                  , nodeAttributes = [Label (StrLabel (fromString (showADL r)))]
                                  }
                        | r<-toRels
                  --TODOHAN     , (not.null) [e |e<-edgesOut, (nodeID . toNode) e==nameONode toRels r ]
                        ]

        --DESCR -> All conjuncts
        conjunctNodes = [ DotNode { nodeID         = nameCNode conjuncts (rul,c)
                                  , nodeAttributes = [Style [SItem Filled []], FillColor [WC(X11Color (colorRule rul))Nothing], Label (StrLabel (fromString (name rul)))]
                                  }
                        | (rul,c)<-conjuncts]

        --DESCR -> Each edge represents an insert between a relation on the left and a term on the right to which the relation contributes to an insert.
        edgesIn       = [ DotEdge { fromNode       = nameINode fromRels r
                                  , toNode         = nameCNode conjuncts (rul,c)
                                  , edgeAttributes = [Label (StrLabel (fromString 
                                                                      (if or (positiveIn c r) then "-" else
                                                                       if or [not b |b<-positiveIn c r] then "+" else
                                                                       "+-")))
                                                     ,Dir Forward]
                                  }
                        | (rul,c)<-conjuncts, r<-relsUsedIn c, r `elem` fromRels]
        edgesOut      = [ DotEdge { fromNode       = nameCNode conjuncts (rul,c)
                                  , toNode         = nameONode toRels r
                                  , edgeAttributes = [Label (StrLabel (fromString
                                                                      (if or (positiveIn c r) then "+" else
                                                                       if or [not b |b<-positiveIn c r] then "-" else
                                                                       "+-")))
                                                     ,Dir Forward]
                                  }
                        | (rul,c)<-conjuncts, r<-relsUsedIn c]
        nameINode :: [Declaration] -> Declaration -> String
        nameINode = nmLkp fSpec "in_"
        nameCNode = nmLkp fSpec "cj_"
        nameONode :: [Declaration] -> Declaration -> String
        nameONode = nmLkp fSpec "out_"


   nmLkp :: (LanguageDependent a, Eq a, ShowADL a) => Fspc -> String -> [a] -> a -> String
   nmLkp _ prefix xs x
    = head ([prefix++show (i::Int) | (i,e)<-zip [1..] xs, e==x]++
            fatal 216 ("illegal lookup in nmLkp "++show prefix++": " ++showADL x++
                       "\nin: ["++intercalate "\n    , " (map showADL xs)++"\n    ]")
           )
   positiveIn :: Expression -> Declaration -> [Bool]
   positiveIn expr decl = f expr   -- all are True, so an insert in rel means an insert in expr
    where
     f (EEqu _)     = fatal 237 "Illegal call of positiveIn."
     f (EImp (l,r)) = f (notCpl l .\/. r)
     f (EIsc (l,r)) = f l ++ f r
     f (EUni (l,r)) = f l ++ f r
     f (EDif (l,r)) = f l ++ f (notCpl r)
     f (ELrs (l,r)) = f l ++ f (notCpl r)
     f (ERrs (l,r)) = f (notCpl l) ++ f r
     f (EDia (l,r)) = f (flp l .\. r ./\. l ./. flp r)
     f (ECps (l,r)) = f l ++ f r
     f (ERad (l,r)) = f l ++ f r
     f (EPrd (l,r)) = f l ++ f r
     f (EKl0 e)     = f e
     f (EKl1 e)     = f e
     f (EFlp e)     = f e
     f (ECpl e)     = [ not b | b<- f e]
     f (EBrk e)     = f e
     f (EDcD d)     = [ True | d==decl ]
     f (EDcI c)     = [ True | detyp decl==c ]
     f EEps{}       = []
     f EDcV{}       = []
     f EMp1{}       = []
