{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.Switchboard
    (SwitchBdDiagram(..),switchboardAct,sbDiagram,processModel) where
-- Go to  http://hackage.haskell.org/package/graphviz  for Graphviz bindings for Haskell.
 
   import Data.GraphViz
   import Data.List
   import DatabaseDesign.Ampersand.Basics        (fatalMsg,Collection(..),Identified(..))
   import DatabaseDesign.Ampersand.ADL1
   import DatabaseDesign.Ampersand.ADL1.P2A_Converters (disambiguate)
   import DatabaseDesign.Ampersand.Classes
   import DatabaseDesign.Ampersand.Fspec.Fspec
   import DatabaseDesign.Ampersand.Fspec.ShowADL (ShowADL(..), LanguageDependent(..))
--   import DatabaseDesign.Ampersand.Fspec.ShowECA (showECA) -- for testing purposes
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.Switchboard"


   data SwitchBdDiagram
    = SBdgrm { sbName     :: String
             , sbdotGraph :: DotGraph String
             }
   instance Identified SwitchBdDiagram where
      name = sbName

   processModel :: FProcess -> DotGraph String
   processModel fp
    = DotGraph { strictGraph = False
               , directedGraph = True
               , graphID = Just (Str "Process Model")
               , graphStatements 
                  = DotStmts { attrStmts = [GraphAttrs [{-Splines SplineEdges,-} RankDir FromLeft]]
                             , subGraphs = []
                             , nodeStmts = activityNodes
                             , edgeStmts = edges
                             }
               }
      where
         activityNodes = [ DotNode { nodeID         = "act_"++name a
                                   , nodeAttributes = [Style [SItem Filled []], FillColor (X11Color Orange), Label (StrLabel (name a))]
                                   }
                         | a<-activities fp]
         edges         = nub
                         [ DotEdge { edgeFromNodeID = "act_"++name from
                                   , edgeToNodeID   = "act_"++name to
                                   , edgeAttributes = [Len 2, Label (StrLabel (show e++" "++name r))]
                                   , directedEdge   = True
                                   }
                         | (from,to,e,r) <- allEdges
                         ]
         allEdges  = nub[(from,to,e,r) | (e,r,from)<-eventsOut, (e',r',to)<-eventsIn, e==e', r==r']
         eventsIn  = [(e,r,act) | act<-activities fp, EUni rul<-[rrexp (actRule act)], let antec = EIsc [d |d<-rul,      isCpl d ], r<-mors antec, e<-evIn r antec]
         eventsOut = [(e,r,act) | act<-activities fp, EUni rul<-[rrexp (actRule act)], let consq = EUni [d |d<-rul, not (isCpl d)], r<-mors consq, e<-evIn r consq]
         evIn r (EIsc es)  = foldr uni [] [evIn r e | e<-es]
         evIn r (EUni es)  = foldr uni [] [evIn r e | e<-es]
         evIn r (ECps es)  = foldr uni [] [evIn r e | e<-es]
         evIn r (ERad es)  = foldr uni [] [evIn r e | e<-es]
         evIn r (EPrd es)  = foldr uni [] [evIn r e | e<-es]
         evIn r (EKl0 e)   = evIn r e
         evIn r (EKl1 e)   = evIn r e
         evIn r (EFlp e)   = evIn r e
         evIn r (ECpl e)   = [case x of Ins->Del; Del->Ins | x<-evIn r e]
         evIn r (EBrk e)   = evIn r e
         evIn r (ETyp e _) = evIn r e
         evIn r (ERel rel) = [Ins | r==rel]
         evIn _ _        = fatal 66 "evIn can only be called on a disjunctive normal form."
         

   colorRule :: Rule -> X11Color
   colorRule r  | isSignal r = Orange
                | otherwise  = Green

   sbDiagram :: Fspc -> Fswitchboard -> SwitchBdDiagram
   sbDiagram fSpec fsb
    = SBdgrm
        { sbName = name fSpec
        , sbdotGraph
           = DotGraph { strictGraph = False
                      , directedGraph = True
                      , graphID = Just (Str "Switchboard")
                      , graphStatements 
                         = DotStmts { attrStmts = [GraphAttrs [Splines SplineEdges, RankDir FromLeft]]
                                    , subGraphs = []
                                    , nodeStmts = inEvNodes++conjNodes++ecaNodes++outEvNods
                                    , edgeStmts = edgesEvCj++edgesCjEc++edgesEcEv
                                    }
                      }
        }
      where
        --DESCR -> The relations from which changes can come
        inEvNodes = [ DotNode { nodeID         = nameINode eventsIn ev
                              , nodeAttributes = [Label (StrLabel (show (eSrt ev)++" "++(showADL . disambiguate fSpec . ERel) (eRel ev)))]
                              }
                    | ev<-eventsIn
                    ]
        --DESCR -> All conjuncts
        conjNodes = [ DotNode { nodeID         = nameCNode (fsbConjs fsb) (rul,c)
                              , nodeAttributes = [Style [SItem Filled []], FillColor (X11Color (colorRule rul)), (Label . StrLabel . name) rul]
                              }
                    | (rul,c)<-fsbConjs fsb]

        --DESCR -> All ECA rules
        ecaNodes  = [ DotNode { nodeID         = nameENode (fsbECAs fsb) eca
                              , nodeAttributes = if isBlk (ecaAction eca)
                                                 then [Style [SItem Filled []], FillColor (X11Color Red), (Label . StrLabel) ("ERR #"++show (ecaNum eca))]
                                                 else [(Label . StrLabel . showADL) eca]
                              }
                    | eca<-fsbECAs fsb, not (isBlk (ecaAction eca))]

        --DESCR -> The relations to which changes are made
        outEvNods = [ DotNode { nodeID         = nameONode eventsOut ev
                              , nodeAttributes = [Label (StrLabel (show (eSrt ev)++" "++(showADL . disambiguate fSpec . ERel) (eRel ev)))]
                              }
                    | ev<-eventsOut
                    ]
        --DESCR -> Each edge represents an insert relation between a morphism on the left and a term on the right to which the relation contributes to an insert.
        edgesEvCj = [ DotEdge { edgeFromNodeID = nameINode eventsIn ev
                              , edgeToNodeID   = nameCNode (fsbConjs fsb) (rul,c)
                              , edgeAttributes = []
                              , directedEdge   = True
                              }
                    | (rul,c)<-fsbConjs fsb, ev<-eventsIn, eRel ev `elem` mors c]
        edgesCjEc = [ DotEdge { edgeFromNodeID = nameCNode (fsbConjs fsb) (rul,c)
                              , edgeToNodeID   = nameENode (fsbECAs fsb) eca
                              , edgeAttributes = []
                              , directedEdge   = True
                              }
                    | (rul,c)<-fsbConjs fsb, eca<-fsbECAs fsb, not (isBlk (ecaAction eca)), rul `elem` concat [r | (_,r)<-paMotiv (ecaAction eca)] ]
        edgesEcEv = nub
                    [ DotEdge { edgeFromNodeID = nameENode (fsbECAs fsb) eca
                              , edgeToNodeID   = nameONode eventsOut (On tOp rel)
                              , edgeAttributes = []
                              , directedEdge   = True
                              }
                    | eca<-fsbECAs fsb, not (isBlk (ecaAction eca))
                    , Do tOp (ERel rel) _ _<-dos (ecaAction eca)
{- for testing purposes:
                    , doAct<-dos (ecaAction eca)
                    , if not (isDo doAct) then fatal 136 ("not in \"Do\" shape: "++showECA fSpec "\n  " doAct) else True
                    , let Do tOp expr _ _=doAct
                          isTm ERel{} = True
                          isTm _    = False
                    , if not (isTm expr) then fatal 138 ("not in \"ERel\" shape: "++showECA fSpec "\n  " doAct) else True
                    , let ERel rel = expr
Note:
The expression 'isDo doAct' will become False if a property is being maintained
(a property is a relation that are both Sym and Asy).   
This situation is implicitly avoided by 'Do tOp (ERel rel) _ _<-dos (ecaAction eca)'.
-}
                    ]
        nameINode = nmLkp fSpec "in_"
        nameCNode = nmLkp fSpec "cj_"
        nameENode = nmLkp fSpec "eca_"
        nameONode = nmLkp fSpec "out_"
        eventsIn  = nub [ecaTriggr eca | eca<-fsbECAs fsb, not (isBlk (ecaAction eca)) ]
        eventsOut = nub [On tOp rel | eca<-fsbECAs fsb, let act=ecaAction eca, not (isBlk act), Do tOp (ERel rel) _ _<-dos act]

   switchboardAct :: Fspc -> Activity -> SwitchBdDiagram
   switchboardAct fSpec act
    = SBdgrm
        { sbName = name act
        , sbdotGraph
           = DotGraph { strictGraph = False
                      , directedGraph = True
                      , graphID = Just (Str "Switchboard")
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
        toRels       = nub (actAffect act)
        conjuncts    = nub [(cl_rule ccrs,c)
                           | Quad _ ccrs<-actQuads act, (c,_)<-cl_conjNF ccrs]
        --DESCR -> The relations from which changes can come
        inMorNodes    = [ DotNode { nodeID         = nameINode fromRels r
                                  , nodeAttributes = [Label (StrLabel ((showADL . disambiguate fSpec . ERel) r))]
                                  }
                        | r<-fromRels
                        , (not.null) [e |e<-edgesIn, edgeFromNodeID e==nameINode fromRels r]
                        ]

        --DESCR -> The relations to which changes are made
        outMorNodes   = [ DotNode { nodeID         = nameONode toRels r
                                  , nodeAttributes = [Label (StrLabel ((showADL . disambiguate fSpec . ERel) r))]
                                  }
                        | r<-toRels
                        , (not.null) [e |e<-edgesOut, edgeToNodeID e==nameONode toRels r ]
                        ]

        --DESCR -> All conjuncts
        conjunctNodes = [ DotNode { nodeID         = nameCNode conjuncts (rul,c)
                                  , nodeAttributes = [Style [SItem Filled []], FillColor (X11Color (colorRule rul)), Label (StrLabel (name rul))]
                                  }
                        | (rul,c)<-conjuncts]

        --DESCR -> Each edge represents an insert relation between a morphism on the left and a term on the right to which the relation contributes to an insert.
        edgesIn       = [ DotEdge { edgeFromNodeID = nameINode fromRels r
                                  , edgeToNodeID   = nameCNode conjuncts (rul,c)
                                  , edgeAttributes = [Label (StrLabel (if or (positiveIn c r) then "-" else
                                                                       if or [not b |b<-positiveIn c r] then "+" else
                                                                       "+-"))]
                                  , directedEdge   = True
                                  }
                        | (rul,c)<-conjuncts, r<-(nub.mors) c, r `elem` fromRels]
        edgesOut      = [ DotEdge { edgeFromNodeID = nameCNode conjuncts (rul,c)
                                  , edgeToNodeID   = nameONode toRels r
                                  , edgeAttributes = [Label (StrLabel (if or (positiveIn c r) then "+" else
                                                                       if or [not b |b<-positiveIn c r] then "-" else
                                                                       "+-"))]
                                  , directedEdge   = True
                                  }
                        | (rul,c)<-conjuncts, r<-(nub.mors) c]
        nameINode = nmLkp fSpec "in_"
        nameCNode = nmLkp fSpec "cj_"
        nameONode = nmLkp fSpec "out_"


   nmLkp :: (LanguageDependent a, Eq a, ShowADL a) => Fspc -> String -> [a] -> a -> String
   nmLkp fSpec prefix xs x
    = head ([prefix++show (i::Int) | (i,e)<-zip [1..] xs, e==x]++
            fatal 216 ("illegal lookup in nmLkp: " ++showADL (mapexprs disambiguate fSpec x)++
                       "\nin: ["++intercalate ", " (map (showADL.mapexprs disambiguate fSpec) xs)++"]")
           )
   positiveIn :: Expression -> Relation -> [Bool]
   positiveIn expr rel = f expr   -- all are True, so an insert in rel means an insert in expr
    where
     f (EEqu _)       = fatal 245 "Illegal call of positiveIn."
     f (EImp (l,r))   = f (EUni [ECpl l,r])
     f (EIsc es)      = concatMap f es
     f (EUni es)      = concatMap f es
     f (EDif (l,r))   = f (EIsc [l,ECpl r])
     f (ELrs (l,r))   = f (ERad [l,ECpl (EFlp r)])
     f (ERrs (l,r))   = f (ERad [ECpl (EFlp l),r])
     f (ECps es)      = concatMap f es
     f (ERad es)      = concatMap f es
     f (EPrd es)      = concatMap f es
     f (EKl0 e)       = f e
     f (EKl1 e)       = f e
     f (EFlp e)       = f e
     f (ECpl e)       = [ not b | b<- f e]
     f (EBrk e)       = f e
     f (ETyp e _)     = f e
     f (ERel r)       = [ True | r==rel ]
     
