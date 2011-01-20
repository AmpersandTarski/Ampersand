{-# OPTIONS_GHC -Wall #-}
module Switchboard(switchboard1,SwitchBoard(..)) where
 
   import Data.GraphViz
   --     --Als de compiler hierover struikelt, dan moet je graphviz installeren. Dat is overigens in de volgende 3 stappen:
   --              -- 1) Eerst installeer je Cabal (zie http://www.haskell.org/cabal/) en dan roep je op je command line: 
   --              -- 2) cabal-install graphviz  (onder windows: cabal install graphviz)
   --              -- 3) er is geen stap 3!
   --              -- 4) build on graphviz-2999.5.0.0
   import Ampersand
   import Data.Fspec
   import Collection (Collection(rd))
   import Calc (positiveIn)
   import ShowADL

   data SwitchBoard = SwitchBoard { sbName :: String
                                  , sbdotGraph :: DotGraph String
                                  }
   instance Identified SwitchBoard where
      name sb = sbName sb
   switchboard1 :: (Identified a,ViewPoint a) => Fspc -> a -> SwitchBoard
   switchboard1 fSpec spc = SwitchBoard (name spc) 
                                        (switchboardzzz fSpec spc)

   switchboardzzz :: ViewPoint a => Fspc -> a -> DotGraph String
   switchboardzzz fSpec spc 
     = DotGraph { strictGraph = False
                , directedGraph = True
                , graphID = Just (Str "Switchboard")
                , graphStatements 
                      = DotStmts { attrStmts = [GraphAttrs [{-Splines SplineEdges,-} RankDir FromLeft]]
                                 , subGraphs = []
                                 , nodeStmts = inMorNodes++outMorNodes++negTermNodes++posTermNodes++conjunctNodes
                                 , edgeStmts = insEdgesIn++violEdgesUp++violEdgesDown++insEdgesOut
                                 }
                }
       where 
         rels         = rd (map makeInline (mors spc))++map mIs (concs spc)
         qs           = vquads fSpec
         conjuncts    = rd [conj   | Quad _ ccrs<-qs, (conj,_)<-cl_conjNF ccrs]
         clauses      = rd [clause | Quad _ ccrs<-qs, (_,shifts)<-cl_conjNF ccrs, clause<-shifts]
         posTerms     = rd [term | Fux terms<-clauses, term<-terms, isPos term]
         negTerms     = rd [notCp term | Fux terms<-clauses, term<-terms, isNeg term]
--         colConj c    = head [ColorName color| (color,conj)<-zip allcolors conjs, c==conj]
--                        where conjs = rd [conj | Quad m ccrs<-qs, (conj,shifts)<-cl_conjNF ccrs]
--                              allcolors = ["Linen", "red", "Green", "Yellow", "Blue", "Green", "Violet", "Blueviolet", "Mistyrose"
--                                          , "Orange", "Skyblue", "Plum", "Orange", "Thistle", "Tan", "SpringGreen"]++allcolors
--         colorCl c    = head [colConj conj | Quad _ ccrs<-qs, (conj,shifts)<-cl_conjNF ccrs, clause<-shifts, c==clause]
         nameConj c   = head [nmConjunct conj | Quad _ ccrs<-qs, (conj,shifts)<-cl_conjNF ccrs, clause<-shifts, c==clause]
         nmConjunct c = head (["conj"  ++show (i::Int)| (i,conj)  <-zip [1..] conjuncts, conj==c]++error("!Fatal (module Switchboard 50): illegal lookup in nmConjunct: "++showADLcode fSpec c))
--         nameClaus c  = head (["clause"++show (i::Int)| (i,clause)<-zip [1..] clauses, clause==c]++error("!Fatal (module Switchboard 51): illegal lookup in nameClaus: " ++showADLcode fSpec c))
         namePTerm t  = head (["pos"   ++show (i::Int)| (i,term)  <-zip [1..] posTerms,  term==t]++error("!Fatal (module Switchboard 52): illegal lookup in namePTerm: " ++showADLcode fSpec t))
         nameNTerm t  = head (["neg"   ++show (i::Int)| (i,term)  <-zip [1..] negTerms,  term==t]++error("!Fatal (module Switchboard 53): illegal lookup in nameNTerm: " ++showADLcode fSpec t))
         nameINode m  = head (["in_"   ++show (i::Int)| (i,mph)  <-zip [1..] rels,  makeInline m==mph]++error("!Fatal (module Switchboard 54): illegal lookup in nameNTerm: " ++showADLcode fSpec m))
         nameONode m  = head (["out_"  ++show (i::Int)| (i,mph)  <-zip [1..] rels,  makeInline m==mph]++error("!Fatal (module Switchboard 55): illegal lookup in nameNTerm: " ++showADLcode fSpec m))
         --DESCR -> The relations from which changes can come
         inMorNodes    = [ DotNode { nodeID         = nameINode m
                                   , nodeAttributes = [Label (StrLabel (showADLcode fSpec (makeInline m)))]
                                   }
                         | m<-mors negTerms  ]

         --DESCR -> The relations to which changes are made
         outMorNodes   = [ DotNode { nodeID         = nameONode m
                                   , nodeAttributes = [Label (StrLabel (showADLcode fSpec (makeInline m)))]
                                   }
                         | m<-mors posTerms ]

         --DESCR -> The negative terms of all clauses
         negTermNodes  = [ DotNode { nodeID         = nameNTerm term
                                   , nodeAttributes = [Label (StrLabel (showADLcode fSpec term))]
                                   }
                         | term<-negTerms]

         --DESCR -> The positive terms of all clauses
         posTermNodes  = [ DotNode { nodeID         = namePTerm term
                                   , nodeAttributes = [Label (StrLabel (showADLcode fSpec term))]
                                   }
                         | term<-posTerms]

         --DESCR -> All conjuncts
         conjunctNodes = [ DotNode { nodeID         = nmConjunct c
                                   , nodeAttributes = [Label (StrLabel (showADLcode fSpec c))]
                                   }
                         | c<-conjuncts]

         --DESCR -> All clauses
 --        clauseNodes   = [ DotNode { nodeID         = nameClaus c
 --                                  , nodeAttributes = [Style [SItem Filled []], FillColor (colorCl c), Label (StrLabel (showADLcode fSpec c))]
 --                                  }
 --                        | c<-clauses]

         --DESCR -> Each edge represents an insert relation between a morphism on the left and a term on the right to which the relation contributes to an insert.
         insEdgesIn    = [ DotEdge { edgeFromNodeID = nameINode m
                                   , edgeToNodeID   = nameNTerm t
                                   , edgeAttributes = []
                                   , directedEdge   = True
                                   }
                         | t<-negTerms, m<-mors t, positiveIn t m==Just True]

         --DESCR -> 
         violEdgesUp   = [ DotEdge { edgeFromNodeID = nameNTerm (notCp t)
                                   , edgeToNodeID   = nameConj c
                                   , edgeAttributes = []
                                   , directedEdge   = True
                                   }
                         | c@(Fux fus)<-clauses, t<-fus, isNeg t]

         --DESCR -> 
         violEdgesDown = [ DotEdge { edgeFromNodeID = nameConj c
                                   , edgeToNodeID   = namePTerm t
                                   , edgeAttributes = []
                                   , directedEdge   = True
                                   }
                         | c@(Fux fus)<-clauses, t<-fus, isPos t]

         --DESCR -> Each edge represents an insert relation between a positive term on the left and a morphism on the right in which an insert will be done.
         insEdgesOut   = [ DotEdge { edgeFromNodeID = namePTerm t
                                   , edgeToNodeID   = nameONode m
                                   , edgeAttributes = []
                                   , directedEdge   = True
                                   }
                         | t<-posTerms, m<-mors t, positiveIn t m==Just True]



-- hulpfuncties, voor tijdelijk. TODO, opschonen
--orange :: Attribute
--orange = Color [ColorName "orange"]
--yellow :: Attribute
--yellow = Color [ColorName "yellow"]
--purple :: Attribute
--purple = Color [ColorName "purple"]
--cyan :: Attribute
--cyan = Color [ColorName "cyan"]
--brown :: Attribute
--brown = Color [ColorName "brown"]
--cgreen :: Attribute
--cgreen = Color [ColorName "green"]
--cred :: Attribute
--cred = Color [ColorName "red"]
