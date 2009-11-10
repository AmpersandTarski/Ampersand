{-# OPTIONS_GHC -Wall #-}
{-Generated code by ADL vs. 0.8.10-457 at Tue Nov 10 10:43:37 West-Europa (standaardtijd) 2009-}
module Atlas.AtlasFspec where
  import UU_Scanner
  --import Classification
  import Typology
  import Adl
  --import ShowHS (showHS)
  import Data.Fspec
  import Data.Plug

--  main :: IO ()
--  main = putStr (showHS "\n  " fSpec_atlas)

  fSpec_atlas :: Fspc
  fSpec_atlas
   = Fspc{ fsfsid = (FS_id "Atlas")
         , themes   = []
         , datasets = [ oDef_Expression
                      , oDef_Concept
                      , oDef_Rule
                      , oDef_Relation
                      , oDef_Pair
                      , oDef_Explanation
                      , oDef_MainPicture
                      , oDef_Atom
                      ]
         , vplugs   = [ 
                      ]
         , plugs    = [ plug_rule
                      , plug_expression
                      , plug_relation
                      , plug_concept
                      , plug_pair
                      , plug_explanation
                      , plug_mainpicture
                      , plug_atom
                      , plug_violates
                      , plug_thepicture
                      , plug_contains
                      , plug_containsconcept
                      ]
         , serviceS = serviceS'
         , serviceG = serviceG'
         , services = [ f_Service_Rules
                      , f_Service_Rule
                      , f_Service_Relations
                      , f_Service_Relation
                      , f_Service_Concepts
                      , f_Service_Concept
                      ]
         , vrules   = [ rule1
                      , rule2
                      , rule3
                      , rule4
                      , rule5
                      , rule6
                      , rule7
                      , rule8
                      , rule9
                      , rule10
                      , rule11
                      , rule12
                      , rule13
                      , rule14
                      ]
         , ecaRules = [ 
                      ]
         , vrels    = [ rel_sourceExpressionConcept
                      , rel_targetExpressionConcept
                      , rel_sourceRuleConcept
                      , rel_targetRuleConcept
                      , rel_sourceRelationConcept
                      , rel_targetRelationConcept
                      , rel_violatesPairRule
                      , rel_explanationRuleExplanation
                      , rel_thepictureMainPictureMainPicture
                      , rel_containsRelationPair
                      , rel_containsConceptAtom
                      ]
         , fsisa = isa'
         , violations = []
         }
     where
      isa' = Isa []
                 [C "Expression" gE [],C "Rule" gE [],C "Relation" gE [],C "Pair" gE [],C "MainPicture" gE [],C "Concept" gE [],C "Explanation" gE [],C "Atom" gE [],S ]
      gE = genEq (typology isa')
 -- ***DATASETS***: 
      oDef_Expression
       = Obj{ objnm = "Expression"
            , objpos = (Nowhere)
            , objctx = (Tm (V [] ((S ,C "Expression" gE []))) )
            , objats = [Obj{ objnm = "sourceConcept"
                           , objpos = (Nowhere)
                           , objctx = (Tm (Mph "source" (FilePos ("atlas.adl",Pos 5 7,"::")) [] ((C "Expression" gE [],C "Concept" gE [])) True rel_sourceExpressionConcept) )
                           , objats = []
                           , objstrs = []}
                       ,Obj{ objnm = "targetConcept"
                           , objpos = (Nowhere)
                           , objctx = (Tm (Mph "target" (FilePos ("atlas.adl",Pos 6 7,"::")) [] ((C "Expression" gE [],C "Concept" gE [])) True rel_targetExpressionConcept) )
                           , objats = []
                           , objstrs = []}]
            , objstrs = []}
      oDef_Concept
       = Obj{ objnm = "Concept"
            , objpos = (Nowhere)
            , objctx = (Tm (V [] ((S ,C "Concept" gE []))) )
            , objats = []
            , objstrs = []}
      oDef_Rule
       = Obj{ objnm = "Rule"
            , objpos = (Nowhere)
            , objctx = (Tm (V [] ((S ,C "Rule" gE []))) )
            , objats = [Obj{ objnm = "sourceConcept"
                           , objpos = (Nowhere)
                           , objctx = (Tm (Mph "source" (FilePos ("atlas.adl",Pos 7 7,"::")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept) )
                           , objats = []
                           , objstrs = []}
                       ,Obj{ objnm = "targetConcept"
                           , objpos = (Nowhere)
                           , objctx = (Tm (Mph "target" (FilePos ("atlas.adl",Pos 8 7,"::")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept) )
                           , objats = []
                           , objstrs = []}
                       ,Obj{ objnm = "explanationExplanation"
                           , objpos = (Nowhere)
                           , objctx = (Tm (Mph "explanation" (FilePos ("atlas.adl",Pos 13 12,"::")) [] ((C "Rule" gE [],C "Explanation" gE [])) True rel_explanationRuleExplanation) )
                           , objats = []
                           , objstrs = []}]
            , objstrs = []}
      oDef_Relation
       = Obj{ objnm = "Relation"
            , objpos = (Nowhere)
            , objctx = (Tm (V [] ((S ,C "Relation" gE []))) )
            , objats = [Obj{ objnm = "sourceConcept"
                           , objpos = (Nowhere)
                           , objctx = (Tm (Mph "source" (FilePos ("atlas.adl",Pos 9 7,"::")) [] ((C "Relation" gE [],C "Concept" gE [])) True rel_sourceRelationConcept) )
                           , objats = []
                           , objstrs = []}
                       ,Obj{ objnm = "targetConcept"
                           , objpos = (Nowhere)
                           , objctx = (Tm (Mph "target" (FilePos ("atlas.adl",Pos 10 7,"::")) [] ((C "Relation" gE [],C "Concept" gE [])) True rel_targetRelationConcept) )
                           , objats = []
                           , objstrs = []}]
            , objstrs = []}
      oDef_Pair
       = Obj{ objnm = "Pair"
            , objpos = (Nowhere)
            , objctx = (Tm (V [] ((S ,C "Pair" gE []))) )
            , objats = []
            , objstrs = []}
      oDef_Explanation
       = Obj{ objnm = "Explanation"
            , objpos = (Nowhere)
            , objctx = (Tm (V [] ((S ,C "Explanation" gE []))) )
            , objats = []
            , objstrs = []}
      oDef_MainPicture
       = Obj{ objnm = "MainPicture"
            , objpos = (Nowhere)
            , objctx = (Tm (V [] ((S ,C "MainPicture" gE []))) )
            , objats = []
            , objstrs = []}
      oDef_Atom
       = Obj{ objnm = "Atom"
            , objpos = (Nowhere)
            , objctx = (Tm (V [] ((S ,C "Atom" gE []))) )
            , objats = []
            , objstrs = []}

 -- ***PLUGS***: 
      plug_rule
       = PlugSql{ fields = [ Fld { fldname = "i"
                                 , fldexpr = Tm (I [] (C "Rule" gE []) (C "Rule" gE []) True) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = True
                                 , fldauto = False
                                 }
                           , Fld { fldname = "source"
                                 , fldexpr = Tm (Mph "source" (FilePos ("atlas.adl",Pos 7 7,"::")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = False
                                 , fldauto = False
                                 }
                           , Fld { fldname = "target"
                                 , fldexpr = Tm (Mph "target" (FilePos ("atlas.adl",Pos 8 7,"::")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = False
                                 , fldauto = False
                                 }
                           , Fld { fldname = "explanation"
                                 , fldexpr = Tm (Mph "explanation" (FilePos ("atlas.adl",Pos 13 12,"::")) [] ((C "Rule" gE [],C "Explanation" gE [])) True rel_explanationRuleExplanation) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = False
                                 , fldauto = False
                                 }
                           ]
                , plname = "rule"
                }
      plug_expression
       = PlugSql{ fields = [ Fld { fldname = "i"
                                 , fldexpr = Tm (I [] (C "Expression" gE []) (C "Expression" gE []) True) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = True
                                 , fldauto = False
                                 }
                           , Fld { fldname = "source"
                                 , fldexpr = Tm (Mph "source" (FilePos ("atlas.adl",Pos 5 7,"::")) [] ((C "Expression" gE [],C "Concept" gE [])) True rel_sourceExpressionConcept) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = False
                                 , fldauto = False
                                 }
                           , Fld { fldname = "target"
                                 , fldexpr = Tm (Mph "target" (FilePos ("atlas.adl",Pos 6 7,"::")) [] ((C "Expression" gE [],C "Concept" gE [])) True rel_targetExpressionConcept) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = False
                                 , fldauto = False
                                 }
                           ]
                , plname = "expression"
                }
      plug_relation
       = PlugSql{ fields = [ Fld { fldname = "i"
                                 , fldexpr = Tm (I [] (C "Relation" gE []) (C "Relation" gE []) True) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = True
                                 , fldauto = False
                                 }
                           , Fld { fldname = "source"
                                 , fldexpr = Tm (Mph "source" (FilePos ("atlas.adl",Pos 9 7,"::")) [] ((C "Relation" gE [],C "Concept" gE [])) True rel_sourceRelationConcept) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = False
                                 , fldauto = False
                                 }
                           , Fld { fldname = "target"
                                 , fldexpr = Tm (Mph "target" (FilePos ("atlas.adl",Pos 10 7,"::")) [] ((C "Relation" gE [],C "Concept" gE [])) True rel_targetRelationConcept) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = False
                                 , fldauto = False
                                 }
                           ]
                , plname = "relation"
                }
      plug_concept
       = PlugSql{ fields = [ Fld { fldname = "i"
                                 , fldexpr = Tm (I [] (C "Concept" gE []) (C "Concept" gE []) True) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = True
                                 , fldauto = False
                                 }
                           ]
                , plname = "concept"
                }
      plug_pair
       = PlugSql{ fields = [ Fld { fldname = "i"
                                 , fldexpr = Tm (I [] (C "Pair" gE []) (C "Pair" gE []) True) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = True
                                 , fldauto = False
                                 }
                           ]
                , plname = "pair"
                }
      plug_explanation
       = PlugSql{ fields = [ Fld { fldname = "i"
                                 , fldexpr = Tm (I [] (C "Explanation" gE []) (C "Explanation" gE []) True) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = True
                                 , fldauto = False
                                 }
                           ]
                , plname = "explanation"
                }
      plug_mainpicture
       = PlugSql{ fields = [ Fld { fldname = "i"
                                 , fldexpr = Tm (I [] (C "MainPicture" gE []) (C "MainPicture" gE []) True) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = True
                                 , fldauto = False
                                 }
                           ]
                , plname = "mainpicture"
                }
      plug_atom
       = PlugSql{ fields = [ Fld { fldname = "i"
                                 , fldexpr = Tm (I [] (C "Atom" gE []) (C "Atom" gE []) True) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = True
                                 , fldauto = False
                                 }
                           ]
                , plname = "atom"
                }
      plug_violates
       = PlugSql{ fields = [ Fld { fldname = "pair"
                                 , fldexpr = Fi [ Tm (I [] (C "Pair" gE []) (C "Pair" gE []) True)    , F [ Tm (Mph "violates" (FilePos ("atlas.adl",Pos 12 9,"::")) [] ((C "Pair" gE [],C "Rule" gE [])) True rel_violatesPairRule)        , Tm (Mph "violates" (FilePos ("atlas.adl",Pos 12 9,"::")) [] ((C "Rule" gE [],C "Pair" gE [])) False rel_violatesPairRule)        ]   ]
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = False
                                 , fldauto = False
                                 }
                           , Fld { fldname = "rule"
                                 , fldexpr = Tm (Mph "violates" (FilePos ("atlas.adl",Pos 12 9,"::")) [] ((C "Pair" gE [],C "Rule" gE [])) True rel_violatesPairRule) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = False
                                 , fldauto = False
                                 }
                           ]
                , plname = "violates"
                }
      plug_thepicture
       = PlugSql{ fields = [ Fld { fldname = "mainpicture"
                                 , fldexpr = Fi [ Tm (I [] (C "MainPicture" gE []) (C "MainPicture" gE []) True)    , F [ Tm (Mph "thepicture" (FilePos ("atlas.adl",Pos 14 11,"::")) [] ((C "MainPicture" gE [],C "MainPicture" gE [])) True rel_thepictureMainPictureMainPicture)        , Tm (Mph "thepicture" (FilePos ("atlas.adl",Pos 14 11,"::")) [] ((C "MainPicture" gE [],C "MainPicture" gE [])) False rel_thepictureMainPictureMainPicture)        ]   ]
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = False
                                 , fldauto = False
                                 }
                           , Fld { fldname = "mainpicture1"
                                 , fldexpr = Tm (Mph "thepicture" (FilePos ("atlas.adl",Pos 14 11,"::")) [] ((C "MainPicture" gE [],C "MainPicture" gE [])) True rel_thepictureMainPictureMainPicture) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = False
                                 , fldauto = False
                                 }
                           ]
                , plname = "thepicture"
                }
      plug_contains
       = PlugSql{ fields = [ Fld { fldname = "relation"
                                 , fldexpr = Fi [ Tm (I [] (C "Relation" gE []) (C "Relation" gE []) True)    , F [ Tm (Mph "contains" (FilePos ("atlas.adl",Pos 16 9,"::")) [] ((C "Relation" gE [],C "Pair" gE [])) True rel_containsRelationPair)        , Tm (Mph "contains" (FilePos ("atlas.adl",Pos 16 9,"::")) [] ((C "Pair" gE [],C "Relation" gE [])) False rel_containsRelationPair)        ]   ]
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = False
                                 , fldauto = False
                                 }
                           , Fld { fldname = "pair"
                                 , fldexpr = Tm (Mph "contains" (FilePos ("atlas.adl",Pos 16 9,"::")) [] ((C "Relation" gE [],C "Pair" gE [])) True rel_containsRelationPair) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = False
                                 , fldauto = False
                                 }
                           ]
                , plname = "contains"
                }
      plug_containsconcept
       = PlugSql{ fields = [ Fld { fldname = "concept"
                                 , fldexpr = Fi [ Tm (I [] (C "Concept" gE []) (C "Concept" gE []) True)    , F [ Tm (Mph "contains" (FilePos ("atlas.adl",Pos 17 9,"::")) [] ((C "Concept" gE [],C "Atom" gE [])) True rel_containsConceptAtom)        , Tm (Mph "contains" (FilePos ("atlas.adl",Pos 17 9,"::")) [] ((C "Atom" gE [],C "Concept" gE [])) False rel_containsConceptAtom)        ]   ]
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = False
                                 , fldauto = False
                                 }
                           , Fld { fldname = "atom"
                                 , fldexpr = Tm (Mph "contains" (FilePos ("atlas.adl",Pos 17 9,"::")) [] ((C "Concept" gE [],C "Atom" gE [])) True rel_containsConceptAtom) 
                                 , fldtype = SQLVarchar 255
                                 , fldnull = False
                                 , flduniq = False
                                 , fldauto = False
                                 }
                           ]
                , plname = "containsconcept"
                }

 -- ***Services S***: 
      serviceS' = [ Obj{ objnm = "Rules"
                      , objpos = (FilePos ("atlas.adl",Pos 20 9,"Rules"))
                      , objctx = (Tm (I [S ] (S ) (S ) True) )
                      , objats = [Obj{ objnm = "Rules"
                                     , objpos = (FilePos ("atlas.adl",Pos 21 8,"Rules"))
                                     , objctx = (Tm (V [S ,C "Rule" gE []] ((S ,C "Rule" gE []))) )
                                     , objats = [Obj{ objnm = "rule"
                                                    , objpos = (FilePos ("atlas.adl",Pos 22 14,"rule"))
                                                    , objctx = (Tm (I [C "Rule" gE []] (C "Rule" gE []) (C "Rule" gE []) True) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "source"
                                                    , objpos = (FilePos ("atlas.adl",Pos 23 14,"source"))
                                                    , objctx = (Tm (Mph "source" (FilePos ("atlas.adl",Pos 23 23,"source")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "target"
                                                    , objpos = (FilePos ("atlas.adl",Pos 24 14,"target"))
                                                    , objctx = (Tm (Mph "target" (FilePos ("atlas.adl",Pos 24 23,"target")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "violations"
                                                    , objpos = (FilePos ("atlas.adl",Pos 25 14,"violations"))
                                                    , objctx = (Tm (Mph "violates" (FilePos ("atlas.adl",Pos 25 27,"violates")) [] ((C "Rule" gE [],C "Pair" gE [])) False rel_violatesPairRule) )
                                                    , objats = []
                                                    , objstrs = []}]
                                     , objstrs = []}
                                 ,Obj{ objnm = "Conceptual diagram"
                                     , objpos = (FilePos ("atlas.adl",Pos 26 8,"Conceptual diagram"))
                                     , objctx = (Tm (V [S ,C "MainPicture" gE []] ((S ,C "MainPicture" gE []))) )
                                     , objats = []
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "Rule"
                      , objpos = (FilePos ("atlas.adl",Pos 28 9,"Rule"))
                      , objctx = (Tm (I [C "Rule" gE []] (C "Rule" gE []) (C "Rule" gE []) True) )
                      , objats = [Obj{ objnm = "source"
                                     , objpos = (FilePos ("atlas.adl",Pos 29 8,"source"))
                                     , objctx = (Tm (Mph "source" (FilePos ("atlas.adl",Pos 29 17,"source")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "target"
                                     , objpos = (FilePos ("atlas.adl",Pos 30 8,"target"))
                                     , objctx = (Tm (Mph "target" (FilePos ("atlas.adl",Pos 30 17,"target")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "violations"
                                     , objpos = (FilePos ("atlas.adl",Pos 31 8,"violations"))
                                     , objctx = (Tm (Mph "violates" (FilePos ("atlas.adl",Pos 31 21,"violates")) [] ((C "Rule" gE [],C "Pair" gE [])) False rel_violatesPairRule) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "explanation"
                                     , objpos = (FilePos ("atlas.adl",Pos 32 8,"explanation"))
                                     , objctx = (Tm (Mph "explanation" (FilePos ("atlas.adl",Pos 32 22,"explanation")) [] ((C "Rule" gE [],C "Explanation" gE [])) True rel_explanationRuleExplanation) )
                                     , objats = []
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "Relations"
                      , objpos = (FilePos ("atlas.adl",Pos 34 9,"Relations"))
                      , objctx = (Tm (I [S ] (S ) (S ) True) )
                      , objats = [Obj{ objnm = "Relations"
                                     , objpos = (FilePos ("atlas.adl",Pos 35 8,"Relations"))
                                     , objctx = (Tm (V [S ,C "Relation" gE []] ((S ,C "Relation" gE []))) )
                                     , objats = []
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "Relation"
                      , objpos = (FilePos ("atlas.adl",Pos 37 9,"Relation"))
                      , objctx = (Tm (I [C "Relation" gE []] (C "Relation" gE []) (C "Relation" gE []) True) )
                      , objats = [Obj{ objnm = "source"
                                     , objpos = (FilePos ("atlas.adl",Pos 38 8,"source"))
                                     , objctx = (Tm (Mph "source" (FilePos ("atlas.adl",Pos 38 17,"source")) [] ((C "Relation" gE [],C "Concept" gE [])) True rel_sourceRelationConcept) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "target"
                                     , objpos = (FilePos ("atlas.adl",Pos 39 8,"target"))
                                     , objctx = (Tm (Mph "target" (FilePos ("atlas.adl",Pos 39 17,"target")) [] ((C "Relation" gE [],C "Concept" gE [])) True rel_targetRelationConcept) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "population"
                                     , objpos = (FilePos ("atlas.adl",Pos 40 8,"population"))
                                     , objctx = (Tm (Mph "contains" (FilePos ("atlas.adl",Pos 40 21,"contains")) [] ((C "Relation" gE [],C "Pair" gE [])) True rel_containsRelationPair) )
                                     , objats = []
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "Concepts"
                      , objpos = (FilePos ("atlas.adl",Pos 42 9,"Concepts"))
                      , objctx = (Tm (I [S ] (S ) (S ) True) )
                      , objats = [Obj{ objnm = "Concepts"
                                     , objpos = (FilePos ("atlas.adl",Pos 43 8,"Concepts"))
                                     , objctx = (Tm (V [S ,C "Concept" gE []] ((S ,C "Concept" gE []))) )
                                     , objats = []
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "Concept"
                      , objpos = (FilePos ("atlas.adl",Pos 45 9,"Concept"))
                      , objctx = (Tm (I [C "Concept" gE []] (C "Concept" gE []) (C "Concept" gE []) True) )
                      , objats = [Obj{ objnm = "population"
                                     , objpos = (FilePos ("atlas.adl",Pos 46 8,"population"))
                                     , objctx = (Tm (Mph "contains" (FilePos ("atlas.adl",Pos 46 21,"contains")) [] ((C "Concept" gE [],C "Atom" gE [])) True rel_containsConceptAtom) )
                                     , objats = []
                                     , objstrs = []}]
                      , objstrs = []}
                   ]
 -- ***Services G***: 
      serviceG' = [ Obj{ objnm = "Expression"
                      , objpos = (Nowhere)
                      , objctx = (Tm (I [C "Expression" gE []] (C "Expression" gE []) (C "Expression" gE []) True) )
                      , objats = [Obj{ objnm = "sourceConcept"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "source" (Nowhere) [C "Expression" gE [],C "Concept" gE []] ((C "Expression" gE [],C "Concept" gE [])) True rel_sourceExpressionConcept) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "targetConcept"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "target" (Nowhere) [C "Expression" gE [],C "Concept" gE []] ((C "Expression" gE [],C "Concept" gE [])) True rel_targetExpressionConcept) )
                                     , objats = []
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "nieuweExpression"
                      , objpos = (Nowhere)
                      , objctx = (Tm (I [C "Expression" gE []] (C "Expression" gE []) (C "Expression" gE []) True) )
                      , objats = [Obj{ objnm = "source"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "source" (Nowhere) [] ((C "Expression" gE [],C "Concept" gE [])) True rel_sourceExpressionConcept) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "target"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "target" (Nowhere) [] ((C "Expression" gE [],C "Concept" gE [])) True rel_targetExpressionConcept) )
                                     , objats = []
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "Expressionen"
                      , objpos = (Nowhere)
                      , objctx = (Tm (I [S ] (S ) (S ) True) )
                      , objats = [Obj{ objnm = "Expressionen"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (V [S ,C "Expression" gE []] ((S ,C "Expression" gE []))) )
                                     , objats = [Obj{ objnm = "nr"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (I [C "Expression" gE []] (C "Expression" gE []) (C "Expression" gE []) True) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "sourceConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "source" (Nowhere) [C "Expression" gE [],C "Concept" gE []] ((C "Expression" gE [],C "Concept" gE [])) True rel_sourceExpressionConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "targetConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "target" (Nowhere) [C "Expression" gE [],C "Concept" gE []] ((C "Expression" gE [],C "Concept" gE [])) True rel_targetExpressionConcept) )
                                                    , objats = []
                                                    , objstrs = []}]
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "Concept"
                      , objpos = (Nowhere)
                      , objctx = (Tm (I [C "Concept" gE []] (C "Concept" gE []) (C "Concept" gE []) True) )
                      , objats = [Obj{ objnm = "containsAtom"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "contains" (Nowhere) [C "Concept" gE [],C "Atom" gE []] ((C "Concept" gE [],C "Atom" gE [])) True rel_containsConceptAtom) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "Expression_of_source"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "source" (Nowhere) [C "Expression" gE [],C "Concept" gE []] ((C "Concept" gE [],C "Expression" gE [])) False rel_sourceExpressionConcept) )
                                     , objats = [Obj{ objnm = "sourceConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "source" (Nowhere) [C "Expression" gE [],C "Concept" gE []] ((C "Expression" gE [],C "Concept" gE [])) True rel_sourceExpressionConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "targetConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "target" (Nowhere) [C "Expression" gE [],C "Concept" gE []] ((C "Expression" gE [],C "Concept" gE [])) True rel_targetExpressionConcept) )
                                                    , objats = []
                                                    , objstrs = []}]
                                     , objstrs = []}
                                 ,Obj{ objnm = "Expression_of_target"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "target" (Nowhere) [C "Expression" gE [],C "Concept" gE []] ((C "Concept" gE [],C "Expression" gE [])) False rel_targetExpressionConcept) )
                                     , objats = [Obj{ objnm = "sourceConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "source" (Nowhere) [C "Expression" gE [],C "Concept" gE []] ((C "Expression" gE [],C "Concept" gE [])) True rel_sourceExpressionConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "targetConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "target" (Nowhere) [C "Expression" gE [],C "Concept" gE []] ((C "Expression" gE [],C "Concept" gE [])) True rel_targetExpressionConcept) )
                                                    , objats = []
                                                    , objstrs = []}]
                                     , objstrs = []}
                                 ,Obj{ objnm = "Rule_of_source"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "source" (Nowhere) [C "Rule" gE [],C "Concept" gE []] ((C "Concept" gE [],C "Rule" gE [])) False rel_sourceRuleConcept) )
                                     , objats = [Obj{ objnm = "sourceConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "source" (Nowhere) [C "Rule" gE [],C "Concept" gE []] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "targetConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "target" (Nowhere) [C "Rule" gE [],C "Concept" gE []] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "explanationExplanation"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "explanation" (Nowhere) [] ((C "Rule" gE [],C "Explanation" gE [])) True rel_explanationRuleExplanation) )
                                                    , objats = []
                                                    , objstrs = []}]
                                     , objstrs = []}
                                 ,Obj{ objnm = "Rule_of_target"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "target" (Nowhere) [C "Rule" gE [],C "Concept" gE []] ((C "Concept" gE [],C "Rule" gE [])) False rel_targetRuleConcept) )
                                     , objats = [Obj{ objnm = "sourceConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "source" (Nowhere) [C "Rule" gE [],C "Concept" gE []] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "targetConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "target" (Nowhere) [C "Rule" gE [],C "Concept" gE []] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "explanationExplanation"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "explanation" (Nowhere) [] ((C "Rule" gE [],C "Explanation" gE [])) True rel_explanationRuleExplanation) )
                                                    , objats = []
                                                    , objstrs = []}]
                                     , objstrs = []}
                                 ,Obj{ objnm = "Relation_of_source"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "source" (Nowhere) [C "Relation" gE [],C "Concept" gE []] ((C "Concept" gE [],C "Relation" gE [])) False rel_sourceRelationConcept) )
                                     , objats = [Obj{ objnm = "sourceConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "source" (Nowhere) [C "Relation" gE [],C "Concept" gE []] ((C "Relation" gE [],C "Concept" gE [])) True rel_sourceRelationConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "targetConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "target" (Nowhere) [C "Relation" gE [],C "Concept" gE []] ((C "Relation" gE [],C "Concept" gE [])) True rel_targetRelationConcept) )
                                                    , objats = []
                                                    , objstrs = []}]
                                     , objstrs = []}
                                 ,Obj{ objnm = "Relation_of_target"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "target" (Nowhere) [C "Relation" gE [],C "Concept" gE []] ((C "Concept" gE [],C "Relation" gE [])) False rel_targetRelationConcept) )
                                     , objats = [Obj{ objnm = "sourceConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "source" (Nowhere) [C "Relation" gE [],C "Concept" gE []] ((C "Relation" gE [],C "Concept" gE [])) True rel_sourceRelationConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "targetConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "target" (Nowhere) [C "Relation" gE [],C "Concept" gE []] ((C "Relation" gE [],C "Concept" gE [])) True rel_targetRelationConcept) )
                                                    , objats = []
                                                    , objstrs = []}]
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "Rule"
                      , objpos = (Nowhere)
                      , objctx = (Tm (I [C "Rule" gE []] (C "Rule" gE []) (C "Rule" gE []) True) )
                      , objats = [Obj{ objnm = "sourceConcept"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "source" (Nowhere) [C "Rule" gE [],C "Concept" gE []] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "targetConcept"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "target" (Nowhere) [C "Rule" gE [],C "Concept" gE []] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "explanationExplanation"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "explanation" (Nowhere) [] ((C "Rule" gE [],C "Explanation" gE [])) True rel_explanationRuleExplanation) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "Pair_of_violates"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "violates" (Nowhere) [] ((C "Rule" gE [],C "Pair" gE [])) False rel_violatesPairRule) )
                                     , objats = []
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "nieuweRule"
                      , objpos = (Nowhere)
                      , objctx = (Tm (I [C "Rule" gE []] (C "Rule" gE []) (C "Rule" gE []) True) )
                      , objats = [Obj{ objnm = "source"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "source" (Nowhere) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "target"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "target" (Nowhere) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "explanation"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "explanation" (Nowhere) [] ((C "Rule" gE [],C "Explanation" gE [])) True rel_explanationRuleExplanation) )
                                     , objats = []
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "Rules"
                      , objpos = (Nowhere)
                      , objctx = (Tm (I [S ] (S ) (S ) True) )
                      , objats = [Obj{ objnm = "Rules"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (V [S ,C "Rule" gE []] ((S ,C "Rule" gE []))) )
                                     , objats = [Obj{ objnm = "nr"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (I [C "Rule" gE []] (C "Rule" gE []) (C "Rule" gE []) True) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "sourceConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "source" (Nowhere) [C "Rule" gE [],C "Concept" gE []] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "targetConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "target" (Nowhere) [C "Rule" gE [],C "Concept" gE []] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "explanationExplanation"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "explanation" (Nowhere) [] ((C "Rule" gE [],C "Explanation" gE [])) True rel_explanationRuleExplanation) )
                                                    , objats = []
                                                    , objstrs = []}]
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "Relation"
                      , objpos = (Nowhere)
                      , objctx = (Tm (I [C "Relation" gE []] (C "Relation" gE []) (C "Relation" gE []) True) )
                      , objats = [Obj{ objnm = "sourceConcept"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "source" (Nowhere) [C "Relation" gE [],C "Concept" gE []] ((C "Relation" gE [],C "Concept" gE [])) True rel_sourceRelationConcept) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "targetConcept"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "target" (Nowhere) [C "Relation" gE [],C "Concept" gE []] ((C "Relation" gE [],C "Concept" gE [])) True rel_targetRelationConcept) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "containsPair"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "contains" (Nowhere) [C "Relation" gE [],C "Pair" gE []] ((C "Relation" gE [],C "Pair" gE [])) True rel_containsRelationPair) )
                                     , objats = []
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "nieuweRelation"
                      , objpos = (Nowhere)
                      , objctx = (Tm (I [C "Relation" gE []] (C "Relation" gE []) (C "Relation" gE []) True) )
                      , objats = [Obj{ objnm = "source"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "source" (Nowhere) [] ((C "Relation" gE [],C "Concept" gE [])) True rel_sourceRelationConcept) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "target"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "target" (Nowhere) [] ((C "Relation" gE [],C "Concept" gE [])) True rel_targetRelationConcept) )
                                     , objats = []
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "Relationen"
                      , objpos = (Nowhere)
                      , objctx = (Tm (I [S ] (S ) (S ) True) )
                      , objats = [Obj{ objnm = "Relationen"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (V [S ,C "Relation" gE []] ((S ,C "Relation" gE []))) )
                                     , objats = [Obj{ objnm = "nr"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (I [C "Relation" gE []] (C "Relation" gE []) (C "Relation" gE []) True) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "sourceConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "source" (Nowhere) [C "Relation" gE [],C "Concept" gE []] ((C "Relation" gE [],C "Concept" gE [])) True rel_sourceRelationConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "targetConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "target" (Nowhere) [C "Relation" gE [],C "Concept" gE []] ((C "Relation" gE [],C "Concept" gE [])) True rel_targetRelationConcept) )
                                                    , objats = []
                                                    , objstrs = []}]
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "Pair"
                      , objpos = (Nowhere)
                      , objctx = (Tm (I [C "Pair" gE []] (C "Pair" gE []) (C "Pair" gE []) True) )
                      , objats = [Obj{ objnm = "violatesRule"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "violates" (Nowhere) [] ((C "Pair" gE [],C "Rule" gE [])) True rel_violatesPairRule) )
                                     , objats = [Obj{ objnm = "sourceConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "source" (Nowhere) [C "Rule" gE [],C "Concept" gE []] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "targetConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "target" (Nowhere) [C "Rule" gE [],C "Concept" gE []] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "explanationExplanation"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "explanation" (Nowhere) [] ((C "Rule" gE [],C "Explanation" gE [])) True rel_explanationRuleExplanation) )
                                                    , objats = []
                                                    , objstrs = []}]
                                     , objstrs = []}
                                 ,Obj{ objnm = "Relation_of_contains"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "contains" (Nowhere) [C "Relation" gE [],C "Pair" gE []] ((C "Pair" gE [],C "Relation" gE [])) False rel_containsRelationPair) )
                                     , objats = [Obj{ objnm = "sourceConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "source" (Nowhere) [C "Relation" gE [],C "Concept" gE []] ((C "Relation" gE [],C "Concept" gE [])) True rel_sourceRelationConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "targetConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "target" (Nowhere) [C "Relation" gE [],C "Concept" gE []] ((C "Relation" gE [],C "Concept" gE [])) True rel_targetRelationConcept) )
                                                    , objats = []
                                                    , objstrs = []}]
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "Explanation"
                      , objpos = (Nowhere)
                      , objctx = (Tm (I [C "Explanation" gE []] (C "Explanation" gE []) (C "Explanation" gE []) True) )
                      , objats = [Obj{ objnm = "Rule_of_explanation"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "explanation" (Nowhere) [] ((C "Explanation" gE [],C "Rule" gE [])) False rel_explanationRuleExplanation) )
                                     , objats = [Obj{ objnm = "sourceConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "source" (Nowhere) [C "Rule" gE [],C "Concept" gE []] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "targetConcept"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "target" (Nowhere) [C "Rule" gE [],C "Concept" gE []] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept) )
                                                    , objats = []
                                                    , objstrs = []}
                                                ,Obj{ objnm = "explanationExplanation"
                                                    , objpos = (Nowhere)
                                                    , objctx = (Tm (Mph "explanation" (Nowhere) [] ((C "Rule" gE [],C "Explanation" gE [])) True rel_explanationRuleExplanation) )
                                                    , objats = []
                                                    , objstrs = []}]
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "MainPicture"
                      , objpos = (Nowhere)
                      , objctx = (Tm (I [C "MainPicture" gE []] (C "MainPicture" gE []) (C "MainPicture" gE []) True) )
                      , objats = [Obj{ objnm = "thepictureMainPicture"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "thepicture" (Nowhere) [] ((C "MainPicture" gE [],C "MainPicture" gE [])) True rel_thepictureMainPictureMainPicture) )
                                     , objats = []
                                     , objstrs = []}
                                 ,Obj{ objnm = "MainPicture_of_thepicture"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "thepicture" (Nowhere) [] ((C "MainPicture" gE [],C "MainPicture" gE [])) False rel_thepictureMainPictureMainPicture) )
                                     , objats = []
                                     , objstrs = []}]
                      , objstrs = []}
                   , Obj{ objnm = "Atom"
                      , objpos = (Nowhere)
                      , objctx = (Tm (I [C "Atom" gE []] (C "Atom" gE []) (C "Atom" gE []) True) )
                      , objats = [Obj{ objnm = "Concept_of_contains"
                                     , objpos = (Nowhere)
                                     , objctx = (Tm (Mph "contains" (Nowhere) [C "Concept" gE [],C "Atom" gE []] ((C "Atom" gE [],C "Concept" gE [])) False rel_containsConceptAtom) )
                                     , objats = []
                                     , objstrs = []}]
                      , objstrs = []}
                   ]
 -- ***Declarations of Services ***: 
      f_Service_Rules
       = Fservice 
              (Obj{ objnm = "Rules"
                  , objpos = (FilePos ("atlas.adl",Pos 20 9,"Rules"))
                  , objctx = (Tm (I [S ] (S ) (S ) True) )
                  , objats = [Obj{ objnm = "Rules"
                                 , objpos = (FilePos ("atlas.adl",Pos 21 8,"Rules"))
                                 , objctx = (Tm (V [S ,C "Rule" gE []] ((S ,C "Rule" gE []))) )
                                 , objats = [Obj{ objnm = "rule"
                                                , objpos = (FilePos ("atlas.adl",Pos 22 14,"rule"))
                                                , objctx = (Tm (I [C "Rule" gE []] (C "Rule" gE []) (C "Rule" gE []) True) )
                                                , objats = []
                                                , objstrs = []}
                                            ,Obj{ objnm = "source"
                                                , objpos = (FilePos ("atlas.adl",Pos 23 14,"source"))
                                                , objctx = (Tm (Mph "source" (FilePos ("atlas.adl",Pos 23 23,"source")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept) )
                                                , objats = []
                                                , objstrs = []}
                                            ,Obj{ objnm = "target"
                                                , objpos = (FilePos ("atlas.adl",Pos 24 14,"target"))
                                                , objctx = (Tm (Mph "target" (FilePos ("atlas.adl",Pos 24 23,"target")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept) )
                                                , objats = []
                                                , objstrs = []}
                                            ,Obj{ objnm = "violations"
                                                , objpos = (FilePos ("atlas.adl",Pos 25 14,"violations"))
                                                , objctx = (Tm (Mph "violates" (FilePos ("atlas.adl",Pos 25 27,"violates")) [] ((C "Rule" gE [],C "Pair" gE [])) False rel_violatesPairRule) )
                                                , objats = []
                                                , objstrs = []}]
                                 , objstrs = []}
                             ,Obj{ objnm = "Conceptual diagram"
                                 , objpos = (FilePos ("atlas.adl",Pos 26 8,"Conceptual diagram"))
                                 , objctx = (Tm (V [S ,C "MainPicture" gE []] ((S ,C "MainPicture" gE []))) )
                                 , objats = []
                                 , objstrs = []}]
                  , objstrs = []})
              [ I [C "Rule" gE []] (C "Rule" gE []) (C "Rule" gE []) True
              , Mph "source" (FilePos ("atlas.adl",Pos 23 23,"source")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept
              , Mph "target" (FilePos ("atlas.adl",Pos 24 23,"target")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept
              , Mph "violates" (FilePos ("atlas.adl",Pos 25 27,"violates")) [] ((C "Rule" gE [],C "Pair" gE [])) False rel_violatesPairRule
              ]
              [ rule5
              , rule6
              , rule7
              , rule8
              , rule14
              ]
              [ 
              ]
              [ 
              ]
              [ Att { fld_name     = "Rules"
                    , fld_expr     = Tm (V [S ,C "Rule" gE []] ((S ,C "Rule" gE []))) 
                    , fld_editable = False
                    , fld_list     = True
                    , fld_must     = True
                    , fld_new      = True
                    , fld_fields   = [ Att { fld_name     = "rule"
                                           , fld_expr     = Tm (I [C "Rule" gE []] (C "Rule" gE []) (C "Rule" gE []) True) 
                                           , fld_mph      = I [C "Rule" gE []] (C "Rule" gE []) (C "Rule" gE []) True
                                           , fld_editable = True
                                           , fld_list     = False
                                           , fld_must     = True
                                           , fld_new      = True
                                           , fld_fields   = []
                                           }
                                     , Att { fld_name     = "source"
                                           , fld_expr     = Tm (Mph "source" (FilePos ("atlas.adl",Pos 23 23,"source")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept) 
                                           , fld_mph      = Mph "source" (FilePos ("atlas.adl",Pos 23 23,"source")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept
                                           , fld_editable = True
                                           , fld_list     = False
                                           , fld_must     = True
                                           , fld_new      = True
                                           , fld_fields   = []
                                           }
                                     , Att { fld_name     = "target"
                                           , fld_expr     = Tm (Mph "target" (FilePos ("atlas.adl",Pos 24 23,"target")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept) 
                                           , fld_mph      = Mph "target" (FilePos ("atlas.adl",Pos 24 23,"target")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept
                                           , fld_editable = True
                                           , fld_list     = False
                                           , fld_must     = True
                                           , fld_new      = True
                                           , fld_fields   = []
                                           }
                                     , Att { fld_name     = "violations"
                                           , fld_expr     = Tm (Mph "violates" (FilePos ("atlas.adl",Pos 25 27,"violates")) [] ((C "Rule" gE [],C "Pair" gE [])) False rel_violatesPairRule) 
                                           , fld_mph      = Mph "violates" (FilePos ("atlas.adl",Pos 25 27,"violates")) [] ((C "Rule" gE [],C "Pair" gE [])) False rel_violatesPairRule
                                           , fld_editable = True
                                           , fld_list     = True
                                           , fld_must     = False
                                           , fld_new      = True
                                           , fld_fields   = []
                                           }
                                     ]
                    }
              , Att { fld_name     = "Conceptual diagram"
                    , fld_expr     = Tm (V [S ,C "MainPicture" gE []] ((S ,C "MainPicture" gE []))) 
                    , fld_editable = False
                    , fld_list     = True
                    , fld_must     = True
                    , fld_new      = True
                    , fld_fields   = []
                    }
              ]
          -- Einde Fservice oDef_Rules
      f_Service_Rule
       = Fservice 
              (Obj{ objnm = "Rule"
                  , objpos = (FilePos ("atlas.adl",Pos 28 9,"Rule"))
                  , objctx = (Tm (I [C "Rule" gE []] (C "Rule" gE []) (C "Rule" gE []) True) )
                  , objats = [Obj{ objnm = "source"
                                 , objpos = (FilePos ("atlas.adl",Pos 29 8,"source"))
                                 , objctx = (Tm (Mph "source" (FilePos ("atlas.adl",Pos 29 17,"source")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept) )
                                 , objats = []
                                 , objstrs = []}
                             ,Obj{ objnm = "target"
                                 , objpos = (FilePos ("atlas.adl",Pos 30 8,"target"))
                                 , objctx = (Tm (Mph "target" (FilePos ("atlas.adl",Pos 30 17,"target")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept) )
                                 , objats = []
                                 , objstrs = []}
                             ,Obj{ objnm = "violations"
                                 , objpos = (FilePos ("atlas.adl",Pos 31 8,"violations"))
                                 , objctx = (Tm (Mph "violates" (FilePos ("atlas.adl",Pos 31 21,"violates")) [] ((C "Rule" gE [],C "Pair" gE [])) False rel_violatesPairRule) )
                                 , objats = []
                                 , objstrs = []}
                             ,Obj{ objnm = "explanation"
                                 , objpos = (FilePos ("atlas.adl",Pos 32 8,"explanation"))
                                 , objctx = (Tm (Mph "explanation" (FilePos ("atlas.adl",Pos 32 22,"explanation")) [] ((C "Rule" gE [],C "Explanation" gE [])) True rel_explanationRuleExplanation) )
                                 , objats = []
                                 , objstrs = []}]
                  , objstrs = []})
              [ Mph "source" (FilePos ("atlas.adl",Pos 29 17,"source")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept
              , Mph "target" (FilePos ("atlas.adl",Pos 30 17,"target")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept
              , Mph "violates" (FilePos ("atlas.adl",Pos 31 21,"violates")) [] ((C "Rule" gE [],C "Pair" gE [])) False rel_violatesPairRule
              , Mph "explanation" (FilePos ("atlas.adl",Pos 32 22,"explanation")) [] ((C "Rule" gE [],C "Explanation" gE [])) True rel_explanationRuleExplanation
              ]
              [ rule5
              , rule6
              , rule7
              , rule8
              , rule13
              , rule14
              ]
              [ 
              ]
              [ 
              ]
              [ Att { fld_name     = "source"
                    , fld_expr     = Tm (Mph "source" (FilePos ("atlas.adl",Pos 29 17,"source")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept) 
                    , fld_mph      = Mph "source" (FilePos ("atlas.adl",Pos 29 17,"source")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept
                    , fld_editable = True
                    , fld_list     = False
                    , fld_must     = True
                    , fld_new      = True
                    , fld_fields   = []
                    }
              , Att { fld_name     = "target"
                    , fld_expr     = Tm (Mph "target" (FilePos ("atlas.adl",Pos 30 17,"target")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept) 
                    , fld_mph      = Mph "target" (FilePos ("atlas.adl",Pos 30 17,"target")) [] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept
                    , fld_editable = True
                    , fld_list     = False
                    , fld_must     = True
                    , fld_new      = True
                    , fld_fields   = []
                    }
              , Att { fld_name     = "violations"
                    , fld_expr     = Tm (Mph "violates" (FilePos ("atlas.adl",Pos 31 21,"violates")) [] ((C "Rule" gE [],C "Pair" gE [])) False rel_violatesPairRule) 
                    , fld_mph      = Mph "violates" (FilePos ("atlas.adl",Pos 31 21,"violates")) [] ((C "Rule" gE [],C "Pair" gE [])) False rel_violatesPairRule
                    , fld_editable = True
                    , fld_list     = True
                    , fld_must     = False
                    , fld_new      = True
                    , fld_fields   = []
                    }
              , Att { fld_name     = "explanation"
                    , fld_expr     = Tm (Mph "explanation" (FilePos ("atlas.adl",Pos 32 22,"explanation")) [] ((C "Rule" gE [],C "Explanation" gE [])) True rel_explanationRuleExplanation) 
                    , fld_mph      = Mph "explanation" (FilePos ("atlas.adl",Pos 32 22,"explanation")) [] ((C "Rule" gE [],C "Explanation" gE [])) True rel_explanationRuleExplanation
                    , fld_editable = True
                    , fld_list     = False
                    , fld_must     = True
                    , fld_new      = True
                    , fld_fields   = []
                    }
              ]
          -- Einde Fservice oDef_Rule
      f_Service_Relations
       = Fservice 
              (Obj{ objnm = "Relations"
                  , objpos = (FilePos ("atlas.adl",Pos 34 9,"Relations"))
                  , objctx = (Tm (I [S ] (S ) (S ) True) )
                  , objats = [Obj{ objnm = "Relations"
                                 , objpos = (FilePos ("atlas.adl",Pos 35 8,"Relations"))
                                 , objctx = (Tm (V [S ,C "Relation" gE []] ((S ,C "Relation" gE []))) )
                                 , objats = []
                                 , objstrs = []}]
                  , objstrs = []})
              [ 
              ]
              [ 
              ]
              [ 
              ]
              [ 
              ]
              [ Att { fld_name     = "Relations"
                    , fld_expr     = Tm (V [S ,C "Relation" gE []] ((S ,C "Relation" gE []))) 
                    , fld_editable = False
                    , fld_list     = True
                    , fld_must     = True
                    , fld_new      = True
                    , fld_fields   = []
                    }
              ]
          -- Einde Fservice oDef_Relations
      f_Service_Relation
       = Fservice 
              (Obj{ objnm = "Relation"
                  , objpos = (FilePos ("atlas.adl",Pos 37 9,"Relation"))
                  , objctx = (Tm (I [C "Relation" gE []] (C "Relation" gE []) (C "Relation" gE []) True) )
                  , objats = [Obj{ objnm = "source"
                                 , objpos = (FilePos ("atlas.adl",Pos 38 8,"source"))
                                 , objctx = (Tm (Mph "source" (FilePos ("atlas.adl",Pos 38 17,"source")) [] ((C "Relation" gE [],C "Concept" gE [])) True rel_sourceRelationConcept) )
                                 , objats = []
                                 , objstrs = []}
                             ,Obj{ objnm = "target"
                                 , objpos = (FilePos ("atlas.adl",Pos 39 8,"target"))
                                 , objctx = (Tm (Mph "target" (FilePos ("atlas.adl",Pos 39 17,"target")) [] ((C "Relation" gE [],C "Concept" gE [])) True rel_targetRelationConcept) )
                                 , objats = []
                                 , objstrs = []}
                             ,Obj{ objnm = "population"
                                 , objpos = (FilePos ("atlas.adl",Pos 40 8,"population"))
                                 , objctx = (Tm (Mph "contains" (FilePos ("atlas.adl",Pos 40 21,"contains")) [] ((C "Relation" gE [],C "Pair" gE [])) True rel_containsRelationPair) )
                                 , objats = []
                                 , objstrs = []}]
                  , objstrs = []})
              [ Mph "source" (FilePos ("atlas.adl",Pos 38 17,"source")) [] ((C "Relation" gE [],C "Concept" gE [])) True rel_sourceRelationConcept
              , Mph "target" (FilePos ("atlas.adl",Pos 39 17,"target")) [] ((C "Relation" gE [],C "Concept" gE [])) True rel_targetRelationConcept
              , Mph "contains" (FilePos ("atlas.adl",Pos 40 21,"contains")) [] ((C "Relation" gE [],C "Pair" gE [])) True rel_containsRelationPair
              ]
              [ rule9
              , rule10
              , rule11
              , rule12
              ]
              [ 
              ]
              [ 
              ]
              [ Att { fld_name     = "source"
                    , fld_expr     = Tm (Mph "source" (FilePos ("atlas.adl",Pos 38 17,"source")) [] ((C "Relation" gE [],C "Concept" gE [])) True rel_sourceRelationConcept) 
                    , fld_mph      = Mph "source" (FilePos ("atlas.adl",Pos 38 17,"source")) [] ((C "Relation" gE [],C "Concept" gE [])) True rel_sourceRelationConcept
                    , fld_editable = True
                    , fld_list     = False
                    , fld_must     = True
                    , fld_new      = True
                    , fld_fields   = []
                    }
              , Att { fld_name     = "target"
                    , fld_expr     = Tm (Mph "target" (FilePos ("atlas.adl",Pos 39 17,"target")) [] ((C "Relation" gE [],C "Concept" gE [])) True rel_targetRelationConcept) 
                    , fld_mph      = Mph "target" (FilePos ("atlas.adl",Pos 39 17,"target")) [] ((C "Relation" gE [],C "Concept" gE [])) True rel_targetRelationConcept
                    , fld_editable = True
                    , fld_list     = False
                    , fld_must     = True
                    , fld_new      = True
                    , fld_fields   = []
                    }
              , Att { fld_name     = "population"
                    , fld_expr     = Tm (Mph "contains" (FilePos ("atlas.adl",Pos 40 21,"contains")) [] ((C "Relation" gE [],C "Pair" gE [])) True rel_containsRelationPair) 
                    , fld_mph      = Mph "contains" (FilePos ("atlas.adl",Pos 40 21,"contains")) [] ((C "Relation" gE [],C "Pair" gE [])) True rel_containsRelationPair
                    , fld_editable = True
                    , fld_list     = True
                    , fld_must     = False
                    , fld_new      = True
                    , fld_fields   = []
                    }
              ]
          -- Einde Fservice oDef_Relation
      f_Service_Concepts
       = Fservice 
              (Obj{ objnm = "Concepts"
                  , objpos = (FilePos ("atlas.adl",Pos 42 9,"Concepts"))
                  , objctx = (Tm (I [S ] (S ) (S ) True) )
                  , objats = [Obj{ objnm = "Concepts"
                                 , objpos = (FilePos ("atlas.adl",Pos 43 8,"Concepts"))
                                 , objctx = (Tm (V [S ,C "Concept" gE []] ((S ,C "Concept" gE []))) )
                                 , objats = []
                                 , objstrs = []}]
                  , objstrs = []})
              [ 
              ]
              [ 
              ]
              [ 
              ]
              [ 
              ]
              [ Att { fld_name     = "Concepts"
                    , fld_expr     = Tm (V [S ,C "Concept" gE []] ((S ,C "Concept" gE []))) 
                    , fld_editable = False
                    , fld_list     = True
                    , fld_must     = True
                    , fld_new      = True
                    , fld_fields   = []
                    }
              ]
          -- Einde Fservice oDef_Concepts
      f_Service_Concept
       = Fservice 
              (Obj{ objnm = "Concept"
                  , objpos = (FilePos ("atlas.adl",Pos 45 9,"Concept"))
                  , objctx = (Tm (I [C "Concept" gE []] (C "Concept" gE []) (C "Concept" gE []) True) )
                  , objats = [Obj{ objnm = "population"
                                 , objpos = (FilePos ("atlas.adl",Pos 46 8,"population"))
                                 , objctx = (Tm (Mph "contains" (FilePos ("atlas.adl",Pos 46 21,"contains")) [] ((C "Concept" gE [],C "Atom" gE [])) True rel_containsConceptAtom) )
                                 , objats = []
                                 , objstrs = []}]
                  , objstrs = []})
              [ Mph "contains" (FilePos ("atlas.adl",Pos 46 21,"contains")) [] ((C "Concept" gE [],C "Atom" gE [])) True rel_containsConceptAtom
              ]
              [ 
              ]
              [ 
              ]
              [ 
              ]
              [ Att { fld_name     = "population"
                    , fld_expr     = Tm (Mph "contains" (FilePos ("atlas.adl",Pos 46 21,"contains")) [] ((C "Concept" gE [],C "Atom" gE [])) True rel_containsConceptAtom) 
                    , fld_mph      = Mph "contains" (FilePos ("atlas.adl",Pos 46 21,"contains")) [] ((C "Concept" gE [],C "Atom" gE [])) True rel_containsConceptAtom
                    , fld_editable = True
                    , fld_list     = True
                    , fld_must     = False
                    , fld_new      = True
                    , fld_fields   = []
                    }
              ]
          -- Einde Fservice oDef_Concept

 -- ***Declarations of RULES ***: 
      rule1
       = Ru{ rrsrt = Implication
           , rrant = (F [ Tm (Mph "source" (FilePos ("atlas.adl",Pos 5 7,"::")) [C "Concept" gE [],C "Expression" gE []] ((C "Concept" gE [],C "Expression" gE [])) False rel_sourceExpressionConcept)   , Tm (Mph "source" (FilePos ("atlas.adl",Pos 5 7,"::")) [C "Expression" gE [],C "Concept" gE []] ((C "Expression" gE [],C "Concept" gE [])) True rel_sourceExpressionConcept)   ])
           , rrfps = (FilePos ("atlas.adl",Pos 5 7,"::"))
           , rrcon = (Tm (I [] (C "Concept" gE []) (C "Concept" gE []) True) )
           , r_cpu = []
           , rrxpl = ""
           , rrtyp = (C "Concept" gE [],C "Concept" gE [])
           , runum = 1
           , r_pat = ""}
      rule2
       = Ru{ rrsrt = Implication
           , rrant = (Tm (I [] (C "Expression" gE []) (C "Expression" gE []) True) )
           , rrfps = (FilePos ("atlas.adl",Pos 5 7,"::"))
           , rrcon = (F [ Tm (Mph "source" (FilePos ("atlas.adl",Pos 5 7,"::")) [C "Expression" gE [],C "Concept" gE []] ((C "Expression" gE [],C "Concept" gE [])) True rel_sourceExpressionConcept)   , Tm (Mph "source" (FilePos ("atlas.adl",Pos 5 7,"::")) [C "Concept" gE [],C "Expression" gE []] ((C "Concept" gE [],C "Expression" gE [])) False rel_sourceExpressionConcept)   ])
           , r_cpu = []
           , rrxpl = ""
           , rrtyp = (C "Expression" gE [],C "Expression" gE [])
           , runum = 2
           , r_pat = ""}
      rule3
       = Ru{ rrsrt = Implication
           , rrant = (F [ Tm (Mph "target" (FilePos ("atlas.adl",Pos 6 7,"::")) [C "Concept" gE [],C "Expression" gE []] ((C "Concept" gE [],C "Expression" gE [])) False rel_targetExpressionConcept)   , Tm (Mph "target" (FilePos ("atlas.adl",Pos 6 7,"::")) [C "Expression" gE [],C "Concept" gE []] ((C "Expression" gE [],C "Concept" gE [])) True rel_targetExpressionConcept)   ])
           , rrfps = (FilePos ("atlas.adl",Pos 6 7,"::"))
           , rrcon = (Tm (I [] (C "Concept" gE []) (C "Concept" gE []) True) )
           , r_cpu = []
           , rrxpl = ""
           , rrtyp = (C "Concept" gE [],C "Concept" gE [])
           , runum = 3
           , r_pat = ""}
      rule4
       = Ru{ rrsrt = Implication
           , rrant = (Tm (I [] (C "Expression" gE []) (C "Expression" gE []) True) )
           , rrfps = (FilePos ("atlas.adl",Pos 6 7,"::"))
           , rrcon = (F [ Tm (Mph "target" (FilePos ("atlas.adl",Pos 6 7,"::")) [C "Expression" gE [],C "Concept" gE []] ((C "Expression" gE [],C "Concept" gE [])) True rel_targetExpressionConcept)   , Tm (Mph "target" (FilePos ("atlas.adl",Pos 6 7,"::")) [C "Concept" gE [],C "Expression" gE []] ((C "Concept" gE [],C "Expression" gE [])) False rel_targetExpressionConcept)   ])
           , r_cpu = []
           , rrxpl = ""
           , rrtyp = (C "Expression" gE [],C "Expression" gE [])
           , runum = 4
           , r_pat = ""}
      rule5
       = Ru{ rrsrt = Implication
           , rrant = (F [ Tm (Mph "source" (FilePos ("atlas.adl",Pos 7 7,"::")) [C "Concept" gE [],C "Rule" gE []] ((C "Concept" gE [],C "Rule" gE [])) False rel_sourceRuleConcept)   , Tm (Mph "source" (FilePos ("atlas.adl",Pos 7 7,"::")) [C "Rule" gE [],C "Concept" gE []] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept)   ])
           , rrfps = (FilePos ("atlas.adl",Pos 7 7,"::"))
           , rrcon = (Tm (I [] (C "Concept" gE []) (C "Concept" gE []) True) )
           , r_cpu = []
           , rrxpl = ""
           , rrtyp = (C "Concept" gE [],C "Concept" gE [])
           , runum = 5
           , r_pat = ""}
      rule6
       = Ru{ rrsrt = Implication
           , rrant = (Tm (I [] (C "Rule" gE []) (C "Rule" gE []) True) )
           , rrfps = (FilePos ("atlas.adl",Pos 7 7,"::"))
           , rrcon = (F [ Tm (Mph "source" (FilePos ("atlas.adl",Pos 7 7,"::")) [C "Rule" gE [],C "Concept" gE []] ((C "Rule" gE [],C "Concept" gE [])) True rel_sourceRuleConcept)   , Tm (Mph "source" (FilePos ("atlas.adl",Pos 7 7,"::")) [C "Concept" gE [],C "Rule" gE []] ((C "Concept" gE [],C "Rule" gE [])) False rel_sourceRuleConcept)   ])
           , r_cpu = []
           , rrxpl = ""
           , rrtyp = (C "Rule" gE [],C "Rule" gE [])
           , runum = 6
           , r_pat = ""}
      rule7
       = Ru{ rrsrt = Implication
           , rrant = (F [ Tm (Mph "target" (FilePos ("atlas.adl",Pos 8 7,"::")) [C "Concept" gE [],C "Rule" gE []] ((C "Concept" gE [],C "Rule" gE [])) False rel_targetRuleConcept)   , Tm (Mph "target" (FilePos ("atlas.adl",Pos 8 7,"::")) [C "Rule" gE [],C "Concept" gE []] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept)   ])
           , rrfps = (FilePos ("atlas.adl",Pos 8 7,"::"))
           , rrcon = (Tm (I [] (C "Concept" gE []) (C "Concept" gE []) True) )
           , r_cpu = []
           , rrxpl = ""
           , rrtyp = (C "Concept" gE [],C "Concept" gE [])
           , runum = 7
           , r_pat = ""}
      rule8
       = Ru{ rrsrt = Implication
           , rrant = (Tm (I [] (C "Rule" gE []) (C "Rule" gE []) True) )
           , rrfps = (FilePos ("atlas.adl",Pos 8 7,"::"))
           , rrcon = (F [ Tm (Mph "target" (FilePos ("atlas.adl",Pos 8 7,"::")) [C "Rule" gE [],C "Concept" gE []] ((C "Rule" gE [],C "Concept" gE [])) True rel_targetRuleConcept)   , Tm (Mph "target" (FilePos ("atlas.adl",Pos 8 7,"::")) [C "Concept" gE [],C "Rule" gE []] ((C "Concept" gE [],C "Rule" gE [])) False rel_targetRuleConcept)   ])
           , r_cpu = []
           , rrxpl = ""
           , rrtyp = (C "Rule" gE [],C "Rule" gE [])
           , runum = 8
           , r_pat = ""}
      rule9
       = Ru{ rrsrt = Implication
           , rrant = (F [ Tm (Mph "source" (FilePos ("atlas.adl",Pos 9 7,"::")) [C "Concept" gE [],C "Relation" gE []] ((C "Concept" gE [],C "Relation" gE [])) False rel_sourceRelationConcept)   , Tm (Mph "source" (FilePos ("atlas.adl",Pos 9 7,"::")) [C "Relation" gE [],C "Concept" gE []] ((C "Relation" gE [],C "Concept" gE [])) True rel_sourceRelationConcept)   ])
           , rrfps = (FilePos ("atlas.adl",Pos 9 7,"::"))
           , rrcon = (Tm (I [] (C "Concept" gE []) (C "Concept" gE []) True) )
           , r_cpu = []
           , rrxpl = ""
           , rrtyp = (C "Concept" gE [],C "Concept" gE [])
           , runum = 9
           , r_pat = ""}
      rule10
       = Ru{ rrsrt = Implication
           , rrant = (Tm (I [] (C "Relation" gE []) (C "Relation" gE []) True) )
           , rrfps = (FilePos ("atlas.adl",Pos 9 7,"::"))
           , rrcon = (F [ Tm (Mph "source" (FilePos ("atlas.adl",Pos 9 7,"::")) [C "Relation" gE [],C "Concept" gE []] ((C "Relation" gE [],C "Concept" gE [])) True rel_sourceRelationConcept)   , Tm (Mph "source" (FilePos ("atlas.adl",Pos 9 7,"::")) [C "Concept" gE [],C "Relation" gE []] ((C "Concept" gE [],C "Relation" gE [])) False rel_sourceRelationConcept)   ])
           , r_cpu = []
           , rrxpl = ""
           , rrtyp = (C "Relation" gE [],C "Relation" gE [])
           , runum = 10
           , r_pat = ""}
      rule11
       = Ru{ rrsrt = Implication
           , rrant = (F [ Tm (Mph "target" (FilePos ("atlas.adl",Pos 10 7,"::")) [C "Concept" gE [],C "Relation" gE []] ((C "Concept" gE [],C "Relation" gE [])) False rel_targetRelationConcept)   , Tm (Mph "target" (FilePos ("atlas.adl",Pos 10 7,"::")) [C "Relation" gE [],C "Concept" gE []] ((C "Relation" gE [],C "Concept" gE [])) True rel_targetRelationConcept)   ])
           , rrfps = (FilePos ("atlas.adl",Pos 10 7,"::"))
           , rrcon = (Tm (I [] (C "Concept" gE []) (C "Concept" gE []) True) )
           , r_cpu = []
           , rrxpl = ""
           , rrtyp = (C "Concept" gE [],C "Concept" gE [])
           , runum = 11
           , r_pat = ""}
      rule12
       = Ru{ rrsrt = Implication
           , rrant = (Tm (I [] (C "Relation" gE []) (C "Relation" gE []) True) )
           , rrfps = (FilePos ("atlas.adl",Pos 10 7,"::"))
           , rrcon = (F [ Tm (Mph "target" (FilePos ("atlas.adl",Pos 10 7,"::")) [C "Relation" gE [],C "Concept" gE []] ((C "Relation" gE [],C "Concept" gE [])) True rel_targetRelationConcept)   , Tm (Mph "target" (FilePos ("atlas.adl",Pos 10 7,"::")) [C "Concept" gE [],C "Relation" gE []] ((C "Concept" gE [],C "Relation" gE [])) False rel_targetRelationConcept)   ])
           , r_cpu = []
           , rrxpl = ""
           , rrtyp = (C "Relation" gE [],C "Relation" gE [])
           , runum = 12
           , r_pat = ""}
      rule13
       = Ru{ rrsrt = Implication
           , rrant = (F [ Tm (Mph "explanation" (FilePos ("atlas.adl",Pos 13 12,"::")) [C "Explanation" gE [],C "Rule" gE []] ((C "Explanation" gE [],C "Rule" gE [])) False rel_explanationRuleExplanation)   , Tm (Mph "explanation" (FilePos ("atlas.adl",Pos 13 12,"::")) [C "Rule" gE [],C "Explanation" gE []] ((C "Rule" gE [],C "Explanation" gE [])) True rel_explanationRuleExplanation)   ])
           , rrfps = (FilePos ("atlas.adl",Pos 13 12,"::"))
           , rrcon = (Tm (I [] (C "Explanation" gE []) (C "Explanation" gE []) True) )
           , r_cpu = []
           , rrxpl = ""
           , rrtyp = (C "Explanation" gE [],C "Explanation" gE [])
           , runum = 13
           , r_pat = ""}
      rule14
       = Ru{ rrsrt = Implication
           , rrant = (Tm (I [] (C "Rule" gE []) (C "Rule" gE []) True) )
           , rrfps = (FilePos ("atlas.adl",Pos 13 12,"::"))
           , rrcon = (F [ Tm (Mph "explanation" (FilePos ("atlas.adl",Pos 13 12,"::")) [C "Rule" gE [],C "Explanation" gE []] ((C "Rule" gE [],C "Explanation" gE [])) True rel_explanationRuleExplanation)   , Tm (Mph "explanation" (FilePos ("atlas.adl",Pos 13 12,"::")) [C "Explanation" gE [],C "Rule" gE []] ((C "Explanation" gE [],C "Rule" gE [])) False rel_explanationRuleExplanation)   ])
           , r_cpu = []
           , rrxpl = ""
           , rrtyp = (C "Rule" gE [],C "Rule" gE [])
           , runum = 14
           , r_pat = ""}

 -- ***Declarations OF RELATIONS ***: 
      rel_sourceExpressionConcept
       = Sgn{ decnm   = "source"
            , desrc   = C "Expression" gE []
            , detgt   = C "Concept" gE []
            , decprps = [Uni,Tot]
            , decprL  = ""
            , decprM  = ""
            , decprR  = ""
            , decpopu = []
            , decexpl = ""
            , decfpos = FilePos ("atlas.adl",Pos 5 7,"::")
            , decid   = 0
            , deciss  = False}
      rel_targetExpressionConcept
       = Sgn{ decnm   = "target"
            , desrc   = C "Expression" gE []
            , detgt   = C "Concept" gE []
            , decprps = [Uni,Tot]
            , decprL  = ""
            , decprM  = ""
            , decprR  = ""
            , decpopu = []
            , decexpl = ""
            , decfpos = FilePos ("atlas.adl",Pos 6 7,"::")
            , decid   = 0
            , deciss  = False}
      rel_sourceRuleConcept
       = Sgn{ decnm   = "source"
            , desrc   = C "Rule" gE []
            , detgt   = C "Concept" gE []
            , decprps = [Uni,Tot]
            , decprL  = ""
            , decprM  = ""
            , decprR  = ""
            , decpopu = [("r;s|-t","A")]
            , decexpl = ""
            , decfpos = FilePos ("atlas.adl",Pos 7 7,"::")
            , decid   = 0
            , deciss  = False}
      rel_targetRuleConcept
       = Sgn{ decnm   = "target"
            , desrc   = C "Rule" gE []
            , detgt   = C "Concept" gE []
            , decprps = [Uni,Tot]
            , decprL  = ""
            , decprM  = ""
            , decprR  = ""
            , decpopu = [("r;s|-t","C")]
            , decexpl = ""
            , decfpos = FilePos ("atlas.adl",Pos 8 7,"::")
            , decid   = 0
            , deciss  = False}
      rel_sourceRelationConcept
       = Sgn{ decnm   = "source"
            , desrc   = C "Relation" gE []
            , detgt   = C "Concept" gE []
            , decprps = [Uni,Tot]
            , decprL  = ""
            , decprM  = ""
            , decprR  = ""
            , decpopu = [("r","A"),("s","B"),("t","A")]
            , decexpl = ""
            , decfpos = FilePos ("atlas.adl",Pos 9 7,"::")
            , decid   = 0
            , deciss  = False}
      rel_targetRelationConcept
       = Sgn{ decnm   = "target"
            , desrc   = C "Relation" gE []
            , detgt   = C "Concept" gE []
            , decprps = [Uni,Tot]
            , decprL  = ""
            , decprM  = ""
            , decprR  = ""
            , decpopu = [("r","B"),("s","C"),("t","C")]
            , decexpl = ""
            , decfpos = FilePos ("atlas.adl",Pos 10 7,"::")
            , decid   = 0
            , deciss  = False}
      rel_violatesPairRule
       = Sgn{ decnm   = "violates"
            , desrc   = C "Pair" gE []
            , detgt   = C "Rule" gE []
            , decprps = []
            , decprL  = ""
            , decprM  = ""
            , decprR  = ""
            , decpopu = []
            , decexpl = ""
            , decfpos = FilePos ("atlas.adl",Pos 12 9,"::")
            , decid   = 0
            , deciss  = False}
      rel_explanationRuleExplanation
       = Sgn{ decnm   = "explanation"
            , desrc   = C "Rule" gE []
            , detgt   = C "Explanation" gE []
            , decprps = [Uni,Tot]
            , decprL  = ""
            , decprM  = ""
            , decprR  = ""
            , decpopu = [("r;s|-t","r;s is a subset of t")]
            , decexpl = ""
            , decfpos = FilePos ("atlas.adl",Pos 13 12,"::")
            , decid   = 0
            , deciss  = False}
      rel_thepictureMainPictureMainPicture
       = Sgn{ decnm   = "thepicture"
            , desrc   = C "MainPicture" gE []
            , detgt   = C "MainPicture" gE []
            , decprps = []
            , decprL  = ""
            , decprM  = ""
            , decprR  = ""
            , decpopu = [("atlas.png","atlas.png")]
            , decexpl = ""
            , decfpos = FilePos ("atlas.adl",Pos 14 11,"::")
            , decid   = 0
            , deciss  = False}
      rel_containsRelationPair
       = Sgn{ decnm   = "contains"
            , desrc   = C "Relation" gE []
            , detgt   = C "Pair" gE []
            , decprps = []
            , decprL  = ""
            , decprM  = ""
            , decprR  = ""
            , decpopu = [("r","(a1,b1)"),("r","(a2,b2)"),("s","(b1,c1)"),("s","(b2,c1)"),("t","(a1,c1)"),("t","(a2,c1)")]
            , decexpl = ""
            , decfpos = FilePos ("atlas.adl",Pos 16 9,"::")
            , decid   = 0
            , deciss  = False}
      rel_containsConceptAtom
       = Sgn{ decnm   = "contains"
            , desrc   = C "Concept" gE []
            , detgt   = C "Atom" gE []
            , decprps = []
            , decprL  = ""
            , decprM  = ""
            , decprR  = ""
            , decpopu = [("A","a1"),("A","a2"),("B","b1"),("B","b2"),("C","c1")]
            , decexpl = ""
            , decfpos = FilePos ("atlas.adl",Pos 17 9,"::")
            , decid   = 0
            , deciss  = False}

