{-# OPTIONS_GHC -Wall #-}
{-Generated code by Ampersand-v5.2.5 [Issue-1534-new-attempt:c99354691*], build time: 08-Mar-25 22:52:52 CET at 2025-03-08 22:04:11.138892089 UTC-}
module Main
where

import Ampersand
import Text.Pandoc hiding (MetaData)

main :: IO ()
main = do env <- getOptions
          say (showHS env "\n  " fSpec_ComparisonTest1)

fSpec_ComparisonTest1 :: FSpec
fSpec_ComparisonTest1 =
  FSpec{ fsName          = ComparisonTest1
       , originalContext = Just ComparisonTest1
       , fspos           = [ FileLoc (/workspaces/Ampersand/testing/Travis/testcases/Check/ComparisonTest1.adl:1:1 "CONTEXT") ]
       , plugInfos       = [ InternalPlug plug_Leeftijd
                           , InternalPlug plug_Persoon
                           , InternalPlug plug_ONE
                           , InternalPlug plug_groterDan
                           , InternalPlug plug_isOuderVan
                           ]
       , interfaceS      = interfaceS'
       , interfaceG      = interfaceG'
       , roleInterfaces  = []
       , fRoleRuls       = []
       , fRoles          = []
       , vrules          = [ rule_kindIsJonger1
                           , rule_kindIsJonger2
                           , rule_kindIsJonger3
                           , rule_kindIsJonger4
                           , rule_ouderIsOuder1
                           , rule_ouderIsOuder2
                           , rule_ouderIsOuder3
                           , rule_ouderIsOuder4
                           ]
       , grules          = [ rule_TOT_3040012754054565971
                           , rule_UNI_3040012754054565971
                           ]
       , invariants      = [ rule_TOT_3040012754054565971
                           , rule_UNI_3040012754054565971
                           , rule_kindIsJonger1
                           , rule_kindIsJonger2
                           , rule_kindIsJonger3
                           , rule_kindIsJonger4
                           , rule_ouderIsOuder1
                           , rule_ouderIsOuder2
                           , rule_ouderIsOuder3
                           , rule_ouderIsOuder4
                           ]
       , fallRules       = [ rule_TOT_3040012754054565971
                           , rule_UNI_3040012754054565971
                           , rule_kindIsJonger1
                           , rule_kindIsJonger2
                           , rule_kindIsJonger3
                           , rule_kindIsJonger4
                           , rule_ouderIsOuder1
                           , rule_ouderIsOuder2
                           , rule_ouderIsOuder3
                           , rule_ouderIsOuder4
                           ]
       , allUsedDecls    = [ rel_groterDan_Leeftijd_Leeftijd
                           , rel_isOuderVan_Persoon_Persoon
                           , rel_leeftijd_Persoon_Leeftijd
                           ]
       , vrels           = [ rel_groterDan_Leeftijd_Leeftijd
                           , rel_isOuderVan_Persoon_Persoon
                           , rel_leeftijd_Persoon_Leeftijd
                           ]
       , allConcepts     = [ cpt_Leeftijd
                           , cpt_Persoon
                           , cptOne
                           ]
       , vIndices        = []
       , vviews          = []
       , vgens           = []
       , fsisa           = []
       , allConjuncts    = [ conj_0
                           , conj_1
                           , conj_2
                           , conj_3
                           , conj_4
                           , conj_5
                           , conj_6
                           , conj_7
                           ]
       , vquads          = [ quad_rel_leeftijd_Persoon_Leeftijd_TOT_3040012754054565971
                           , quad_rel_leeftijd_Persoon_Leeftijd_UNI_3040012754054565971
                           , quad_rel_groterDan_Leeftijd_Leeftijd_kindIsJonger1
                           , quad_rel_isOuderVan_Persoon_Persoon_kindIsJonger1
                           , quad_rel_leeftijd_Persoon_Leeftijd_kindIsJonger1
                           , quad_rel_isOuderVan_Persoon_Persoon_kindIsJonger2
                           , quad_rel_leeftijd_Persoon_Leeftijd_kindIsJonger2
                           , quad_rel_groterDan_Leeftijd_Leeftijd_kindIsJonger3
                           , quad_rel_isOuderVan_Persoon_Persoon_kindIsJonger3
                           , quad_rel_leeftijd_Persoon_Leeftijd_kindIsJonger3
                           , quad_rel_isOuderVan_Persoon_Persoon_kindIsJonger4
                           , quad_rel_leeftijd_Persoon_Leeftijd_kindIsJonger4
                           , quad_rel_groterDan_Leeftijd_Leeftijd_ouderIsOuder1
                           , quad_rel_isOuderVan_Persoon_Persoon_ouderIsOuder1
                           , quad_rel_leeftijd_Persoon_Leeftijd_ouderIsOuder1
                           , quad_rel_isOuderVan_Persoon_Persoon_ouderIsOuder2
                           , quad_rel_leeftijd_Persoon_Leeftijd_ouderIsOuder2
                           , quad_rel_groterDan_Leeftijd_Leeftijd_ouderIsOuder3
                           , quad_rel_isOuderVan_Persoon_Persoon_ouderIsOuder3
                           , quad_rel_leeftijd_Persoon_Leeftijd_ouderIsOuder3
                           , quad_rel_isOuderVan_Persoon_Persoon_ouderIsOuder4
                           , quad_rel_leeftijd_Persoon_Leeftijd_ouderIsOuder4
                           ]
       , vpatterns       = []
       , conceptDefs     = [ AConceptDef { pos = FileLoc (/workspaces/Ampersand/testing/Travis/testcases/Check/ComparisonTest1.adl:3:1 ())
                                         , acdcpt = Persoon
                                         , acddef2 = Meaning Markup{ amLang   = Dutch                                        , amPandoc = Many {unMany = fromList []}                                        }
                                         , acdmean = []
                                         , acdfrom = ComparisonTest1
                           , AConceptDef { pos = FileLoc (/workspaces/Ampersand/testing/Travis/testcases/Check/ComparisonTest1.adl:5:1 ())
                                         , acdcpt = Leeftijd
                                         , acddef2 = Meaning Markup{ amLang   = Dutch                                        , amPandoc = Many {unMany = fromList []}                                        }
                                         , acdmean = []
                                         , acdfrom = ComparisonTest1
                           ]
       , fSexpls         = []
       , metas           = allMetas
       , allViolations   = []
       , allExprs        = [ EInc (ECps (ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                              ,ECpl (EDcI cpt_Leeftijd)
                                              )
                                        ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                        )
                                  ,ECpl (EDcI cpt_Persoon)
                                  )
                           , EInc (EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                                  ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                        ,ECps (ECpl (EDcD rel_groterDan_Leeftijd_Leeftijd)
                                              ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                              )
                                        )
                                  )
                           , EInc (EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                                  ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                        ,ECps (EBin <cpt_Leeftijd
                                              ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                              )
                                        )
                                  )
                           , EInc (EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                                  ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                        ,ECps (EBin <=cpt_Leeftijd
                                              ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                              )
                                        )
                                  )
                           , EInc (EDcD rel_isOuderVan_Persoon_Persoon
                                  ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                        ,ECps (EDcD rel_groterDan_Leeftijd_Leeftijd
                                              ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                              )
                                        )
                                  )
                           , EInc (EDcD rel_isOuderVan_Persoon_Persoon
                                  ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                        ,ECps (EBin >cpt_Leeftijd
                                              ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                              )
                                        )
                                  )
                           , EInc (EDcD rel_isOuderVan_Persoon_Persoon
                                  ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                        ,ECps (EBin >=cpt_Leeftijd
                                              ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                              )
                                        )
                                  )
                           , EInc (EDcI cpt_Persoon
                                  ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                        ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                        )
                                  )
                           , EUni (ECpl (ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                              ,ECps (ECpl (EDcI cpt_Leeftijd)
                                                    ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                                    )
                                              ))
                                  ,ECpl (EDcI cpt_Persoon)
                                  )
                           , EUni (ECpl (EFlp (EDcD rel_isOuderVan_Persoon_Persoon))
                                  ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                        ,ECps (ECpl (EDcD rel_groterDan_Leeftijd_Leeftijd)
                                              ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                              )
                                        )
                                  )
                           , EUni (ECpl (EFlp (EDcD rel_isOuderVan_Persoon_Persoon))
                                  ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                        ,ECps (EBin <cpt_Leeftijd
                                              ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                              )
                                        )
                                  )
                           , EUni (ECpl (EFlp (EDcD rel_isOuderVan_Persoon_Persoon))
                                  ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                        ,ECps (EBin <=cpt_Leeftijd
                                              ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                              )
                                        )
                                  )
                           , EUni (ECpl (EDcD rel_isOuderVan_Persoon_Persoon)
                                  ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                        ,ECps (EDcD rel_groterDan_Leeftijd_Leeftijd
                                              ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                              )
                                        )
                                  )
                           , EUni (ECpl (EDcD rel_isOuderVan_Persoon_Persoon)
                                  ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                        ,ECps (EBin >cpt_Leeftijd
                                              ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                              )
                                        )
                                  )
                           , EUni (ECpl (EDcD rel_isOuderVan_Persoon_Persoon)
                                  ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                        ,ECps (EBin >=cpt_Leeftijd
                                              ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                              )
                                        )
                                  )
                           , EUni (ECpl (EDcI cpt_Persoon)
                                  ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                        ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                        )
                                  )
                           , ECps (ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                        ,ECpl (EDcI cpt_Leeftijd)
                                        )
                                  ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                  )
                           , ECps (ECpl (EDcD rel_groterDan_Leeftijd_Leeftijd)
                                  ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                  )
                           , ECps (ECpl (EDcI cpt_Leeftijd)
                                  ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                  )
                           , ECps (EDcD rel_groterDan_Leeftijd_Leeftijd
                                  ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                  )
                           , ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                  ,ECps (ECpl (EDcD rel_groterDan_Leeftijd_Leeftijd)
                                        ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                        )
                                  )
                           , ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                  ,ECps (ECpl (EDcI cpt_Leeftijd)
                                        ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                        )
                                  )
                           , ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                  ,ECps (EDcD rel_groterDan_Leeftijd_Leeftijd
                                        ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                        )
                                  )
                           , ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                  ,ECps (EBin <cpt_Leeftijd
                                        ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                        )
                                  )
                           , ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                  ,ECps (EBin >cpt_Leeftijd
                                        ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                        )
                                  )
                           , ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                  ,ECps (EBin <=cpt_Leeftijd
                                        ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                        )
                                  )
                           , ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                  ,ECps (EBin >=cpt_Leeftijd
                                        ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                        )
                                  )
                           , ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                  ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                  )
                           , ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                  ,ECpl (EDcI cpt_Leeftijd)
                                  )
                           , ECps (EBin <cpt_Leeftijd
                                  ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                  )
                           , ECps (EBin >cpt_Leeftijd
                                  ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                  )
                           , ECps (EBin <=cpt_Leeftijd
                                  ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                  )
                           , ECps (EBin >=cpt_Leeftijd
                                  ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                  )
                           , EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                           , EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                           , ECpl (ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                        ,ECps (ECpl (EDcI cpt_Leeftijd)
                                              ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                              )
                                        ))
                           , ECpl (EFlp (EDcD rel_isOuderVan_Persoon_Persoon))
                           , ECpl (EDcD rel_groterDan_Leeftijd_Leeftijd)
                           , ECpl (EDcD rel_isOuderVan_Persoon_Persoon)
                           , ECpl (EDcI cpt_Leeftijd)
                           , ECpl (EDcI cpt_Persoon)
                           , EDcD rel_groterDan_Leeftijd_Leeftijd
                           , EDcD rel_isOuderVan_Persoon_Persoon
                           , EDcD rel_leeftijd_Persoon_Leeftijd
                           , EDcI cpt_Leeftijd
                           , EDcI cpt_Persoon
                           , EBin <cpt_Leeftijd
                           , EBin >cpt_Leeftijd
                           , EBin <=cpt_Leeftijd
                           , EBin >=cpt_Leeftijd
                           ]
       }
  where
 -- ***Interfaces Specified in Ampersand script***: 
   interfaceS' = []
 -- ***Activities Generated by the Ampersand compiler ***: 
   interfaceG' = []
   allMetas = []
 -- *** Declared relations (in total: 3 relations) ***: 
   rel_groterDan_Leeftijd_Leeftijd
    = Relation { decnm   = groterDan
               , decsgn  = Sign cpt_Leeftijd cpt_Leeftijd
               , decprps = []
               , decpr   = Nothing
               , decMean = []
               , decfpos = FileLoc (/workspaces/Ampersand/testing/Travis/testcases/Check/ComparisonTest1.adl:11:1 ())
               , decusr  = True
               , decpat  = Nothing}
   rel_isOuderVan_Persoon_Persoon
    = Relation { decnm   = isOuderVan
               , decsgn  = Sign cpt_Persoon cpt_Persoon
               , decprps = []
               , decpr   = Nothing
               , decMean = []
               , decfpos = FileLoc (/workspaces/Ampersand/testing/Travis/testcases/Check/ComparisonTest1.adl:9:1 ())
               , decusr  = True
               , decpat  = Nothing}
   rel_leeftijd_Persoon_Leeftijd
    = Relation { decnm   = leeftijd
               , decsgn  = Sign cpt_Persoon cpt_Leeftijd
               , decprps = [Uni,Tot]
               , decpr   = Nothing
               , decMean = []
               , decfpos = FileLoc (/workspaces/Ampersand/testing/Travis/testcases/Check/ComparisonTest1.adl:8:1 ())
               , decusr  = True
               , decpat  = Nothing}

 -- *** User defined rules (total: 8 rules) ***: 
   rule_kindIsJonger1
    = Ru{ rrnm   = kindIsJonger1
        , formalExpression  = -- isOuderVan [Persoon*Persoon]~ |- leeftijd [Persoon*Leeftijd]; -groterDan [Leeftijd*Leeftijd];leeftijd [Persoon*Leeftijd]~
                   EInc (EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                        ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                              ,ECps (ECpl (EDcD rel_groterDan_Leeftijd_Leeftijd)
                                    ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                    )
                              )
                        )
        , rrfps  = FileLoc (/workspaces/Ampersand/testing/Travis/testcases/Check/ComparisonTest1.adl:26:1 ())
        , rrmean = []
        , rrmsg  = []
        , rrviol = Nothing
        , rrpat  = Nothing
        , rrkind  = UserDefined
        }
   rule_kindIsJonger2
    = Ru{ rrnm   = kindIsJonger2
        , formalExpression  = -- isOuderVan [Persoon*Persoon]~ |- leeftijd [Persoon*Leeftijd];<[Leeftijd];leeftijd [Persoon*Leeftijd]~
                   EInc (EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                        ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                              ,ECps (EBin <cpt_Leeftijd
                                    ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                    )
                              )
                        )
        , rrfps  = FileLoc (/workspaces/Ampersand/testing/Travis/testcases/Check/ComparisonTest1.adl:27:1 ())
        , rrmean = []
        , rrmsg  = []
        , rrviol = Nothing
        , rrpat  = Nothing
        , rrkind  = UserDefined
        }
   rule_kindIsJonger3
    = Ru{ rrnm   = kindIsJonger3
        , formalExpression  = -- isOuderVan [Persoon*Persoon]~ |- leeftijd [Persoon*Leeftijd]; -groterDan [Leeftijd*Leeftijd];leeftijd [Persoon*Leeftijd]~
                   EInc (EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                        ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                              ,ECps (ECpl (EDcD rel_groterDan_Leeftijd_Leeftijd)
                                    ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                    )
                              )
                        )
        , rrfps  = FileLoc (/workspaces/Ampersand/testing/Travis/testcases/Check/ComparisonTest1.adl:33:1 ())
        , rrmean = []
        , rrmsg  = []
        , rrviol = Nothing
        , rrpat  = Nothing
        , rrkind  = UserDefined
        }
   rule_kindIsJonger4
    = Ru{ rrnm   = kindIsJonger4
        , formalExpression  = -- isOuderVan [Persoon*Persoon]~ |- leeftijd [Persoon*Leeftijd];<=[Leeftijd];leeftijd [Persoon*Leeftijd]~
                   EInc (EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                        ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                              ,ECps (EBin <=cpt_Leeftijd
                                    ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                    )
                              )
                        )
        , rrfps  = FileLoc (/workspaces/Ampersand/testing/Travis/testcases/Check/ComparisonTest1.adl:34:1 ())
        , rrmean = []
        , rrmsg  = []
        , rrviol = Nothing
        , rrpat  = Nothing
        , rrkind  = UserDefined
        }
   rule_ouderIsOuder1
    = Ru{ rrnm   = ouderIsOuder1
        , formalExpression  = -- isOuderVan [Persoon*Persoon] |- leeftijd [Persoon*Leeftijd];groterDan [Leeftijd*Leeftijd];leeftijd [Persoon*Leeftijd]~
                   EInc (EDcD rel_isOuderVan_Persoon_Persoon
                        ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                              ,ECps (EDcD rel_groterDan_Leeftijd_Leeftijd
                                    ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                    )
                              )
                        )
        , rrfps  = FileLoc (/workspaces/Ampersand/testing/Travis/testcases/Check/ComparisonTest1.adl:23:1 ())
        , rrmean = []
        , rrmsg  = []
        , rrviol = Nothing
        , rrpat  = Nothing
        , rrkind  = UserDefined
        }
   rule_ouderIsOuder2
    = Ru{ rrnm   = ouderIsOuder2
        , formalExpression  = -- isOuderVan [Persoon*Persoon] |- leeftijd [Persoon*Leeftijd];>[Leeftijd];leeftijd [Persoon*Leeftijd]~
                   EInc (EDcD rel_isOuderVan_Persoon_Persoon
                        ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                              ,ECps (EBin >cpt_Leeftijd
                                    ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                    )
                              )
                        )
        , rrfps  = FileLoc (/workspaces/Ampersand/testing/Travis/testcases/Check/ComparisonTest1.adl:24:1 ())
        , rrmean = []
        , rrmsg  = []
        , rrviol = Nothing
        , rrpat  = Nothing
        , rrkind  = UserDefined
        }
   rule_ouderIsOuder3
    = Ru{ rrnm   = ouderIsOuder3
        , formalExpression  = -- isOuderVan [Persoon*Persoon] |- leeftijd [Persoon*Leeftijd];groterDan [Leeftijd*Leeftijd];leeftijd [Persoon*Leeftijd]~
                   EInc (EDcD rel_isOuderVan_Persoon_Persoon
                        ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                              ,ECps (EDcD rel_groterDan_Leeftijd_Leeftijd
                                    ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                    )
                              )
                        )
        , rrfps  = FileLoc (/workspaces/Ampersand/testing/Travis/testcases/Check/ComparisonTest1.adl:30:1 ())
        , rrmean = []
        , rrmsg  = []
        , rrviol = Nothing
        , rrpat  = Nothing
        , rrkind  = UserDefined
        }
   rule_ouderIsOuder4
    = Ru{ rrnm   = ouderIsOuder4
        , formalExpression  = -- isOuderVan [Persoon*Persoon] |- leeftijd [Persoon*Leeftijd];>=[Leeftijd];leeftijd [Persoon*Leeftijd]~
                   EInc (EDcD rel_isOuderVan_Persoon_Persoon
                        ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                              ,ECps (EBin >=cpt_Leeftijd
                                    ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                    )
                              )
                        )
        , rrfps  = FileLoc (/workspaces/Ampersand/testing/Travis/testcases/Check/ComparisonTest1.adl:31:1 ())
        , rrmean = []
        , rrmsg  = []
        , rrviol = Nothing
        , rrpat  = Nothing
        , rrkind  = UserDefined
        }

 -- *** Generated rules (total: 2 rules) ***: 
   rule_TOT_3040012754054565971
    = Ru{ rrnm   = TOT_3040012754054565971
        , formalExpression  = -- I[Persoon] |- leeftijd [Persoon*Leeftijd];leeftijd [Persoon*Leeftijd]~
                   EInc (EDcI cpt_Persoon
                        ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                              ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                              )
                        )
        , rrfps  = PropertyRule "TOT_leeftijd[Persoon*Leeftijd]" (FileLoc (/workspaces/Ampersand/testing/Travis/testcases/Check/ComparisonTest1.adl:8:1 ()))
        , rrmean = [ Meaning Markup{ amLang   = English
                                      , amPandoc = Many {unMany = fromList [Para [Str "leeftijd[Persoon*Leeftijd]",Space,Str "is",Space,Str "total"]]}
                                      }
                    , Meaning Markup{ amLang   = Dutch
                                      , amPandoc = Many {unMany = fromList [Para [Str "leeftijd[Persoon*Leeftijd]",Space,Str "is",Space,Str "totaal"]]}
                                      }
                    ]
        , rrmsg  = [ Markup{ amLang   = English           , amPandoc = Many {unMany = fromList [Para [Str "Every",Space,Str "Persoon",Space,Str "must",Space,Str "have",Space,Str "a",Space,Str "Leeftijd",Space,Str "in",Space,Str "the",Space,Str "relation",Space,Str "leeftijd"]]}           } , Markup{ amLang   = Dutch           , amPandoc = Many {unMany = fromList [Para [Str "Elke",Space,Str "Persoon",Space,Str "dient",Space,Str "\233\233n",Space,Str "Leeftijd",Space,Str "hebben",Space,Str "in",Space,Str "de",Space,Str "relatie",Space,Str "leeftijd"]]}           } ]
        , rrviol = Nothing
        , rrpat  = Nothing
        , rrkind  = Propty Tot, rel_leeftijd_Persoon_Leeftijd
        }
   rule_UNI_3040012754054565971
    = Ru{ rrnm   = UNI_3040012754054565971
        , formalExpression  = -- leeftijd [Persoon*Leeftijd]; -I[Leeftijd];leeftijd [Persoon*Leeftijd]~ |-  -I[Persoon]
                   EInc (ECps (ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                    ,ECpl (EDcI cpt_Leeftijd)
                                    )
                              ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                              )
                        ,ECpl (EDcI cpt_Persoon)
                        )
        , rrfps  = PropertyRule "UNI_leeftijd[Persoon*Leeftijd]" (FileLoc (/workspaces/Ampersand/testing/Travis/testcases/Check/ComparisonTest1.adl:8:1 ()))
        , rrmean = [ Meaning Markup{ amLang   = English
                                      , amPandoc = Many {unMany = fromList [Para [Str "leeftijd[Persoon*Leeftijd]",Space,Str "is",Space,Str "univalent"]]}
                                      }
                    , Meaning Markup{ amLang   = Dutch
                                      , amPandoc = Many {unMany = fromList [Para [Str "leeftijd[Persoon*Leeftijd]",Space,Str "is",Space,Str "univalent"]]}
                                      }
                    ]
        , rrmsg  = [ Markup{ amLang   = English           , amPandoc = Many {unMany = fromList [Para [Str "Each",Space,Str "Persoon",Space,Str "may",Space,Str "only",Space,Str "have",Space,Str "one",Space,Str "Leeftijd",Space,Str "in",Space,Str "the",Space,Str "relation",Space,Str "leeftijd"]]}           } , Markup{ amLang   = Dutch           , amPandoc = Many {unMany = fromList [Para [Str "Elke",Space,Str "Persoon",Space,Str "mag",Space,Str "slechts",Space,Str "\233\233n",Space,Str "Leeftijd",Space,Str "hebben",Space,Str "in",Space,Str "de",Space,Str "relatie",Space,Str "leeftijd"]]}           } ]
        , rrviol = Nothing
        , rrpat  = Nothing
        , rrkind  = Propty Uni, rel_leeftijd_Persoon_Leeftijd
        }

 -- *** Conjuncts (total: 8 conjuncts) ***: 
   conj_0
    = Cjct{ rc_id         = "conj_0"
          , rc_orgRules   = [ rule_UNI_3040012754054565971]
          , rcConjunct   = EUni (ECpl (ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                           ,ECps (ECpl (EDcI cpt_Leeftijd)
                                                 ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                                 )
                                           ))
                               ,ECpl (EDcI cpt_Persoon)
                               )
          , rc_dnfClauses = [ Dnf [ ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                       ,ECps (ECpl (EDcI cpt_Leeftijd)
                                             ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                             )
                                       )
                                , EDcI cpt_Persoon
                                ]
                                [] ]
          }
   conj_1
    = Cjct{ rc_id         = "conj_1"
          , rc_orgRules   = [ rule_kindIsJonger1, rule_kindIsJonger3]
          , rcConjunct   = EUni (ECpl (EFlp (EDcD rel_isOuderVan_Persoon_Persoon))
                               ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                     ,ECps (ECpl (EDcD rel_groterDan_Leeftijd_Leeftijd)
                                           ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                           )
                                     )
                               )
          , rc_dnfClauses = [ Dnf [ EFlp (EDcD rel_isOuderVan_Persoon_Persoon) ]
                                [ ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                       ,ECps (ECpl (EDcD rel_groterDan_Leeftijd_Leeftijd)
                                             ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                             )
                                       ) ]
                          , Dnf [ ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ,EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                                       ) ]
                                [ ECps (ECpl (EDcD rel_groterDan_Leeftijd_Leeftijd)
                                       ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ) ]
                          , Dnf [ ECps (ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                             ,EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                                             )
                                       ,EDcD rel_leeftijd_Persoon_Leeftijd
                                       ) ]
                                [ ECpl (EDcD rel_groterDan_Leeftijd_Leeftijd) ]
                          , Dnf [ ECps (EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                                       ,EDcD rel_leeftijd_Persoon_Leeftijd
                                       ) ]
                                [ ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                       ,ECpl (EDcD rel_groterDan_Leeftijd_Leeftijd)
                                       ) ]
                          , Dnf [ ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ,ECps (EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                                             ,EDcD rel_leeftijd_Persoon_Leeftijd
                                             )
                                       ) ]
                                [ ECpl (EDcD rel_groterDan_Leeftijd_Leeftijd) ]
                          ]
          }
   conj_2
    = Cjct{ rc_id         = "conj_2"
          , rc_orgRules   = [ rule_kindIsJonger2]
          , rcConjunct   = EUni (ECpl (EFlp (EDcD rel_isOuderVan_Persoon_Persoon))
                               ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                     ,ECps (EBin <cpt_Leeftijd
                                           ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                           )
                                     )
                               )
          , rc_dnfClauses = [ Dnf [ EFlp (EDcD rel_isOuderVan_Persoon_Persoon) ]
                                [ ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                       ,ECps (EBin <cpt_Leeftijd
                                             ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                             )
                                       ) ]
                          , Dnf [ ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ,EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                                       ) ]
                                [ ECps (EBin <cpt_Leeftijd
                                       ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ) ]
                          , Dnf [ ECps (ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                             ,EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                                             )
                                       ,EDcD rel_leeftijd_Persoon_Leeftijd
                                       ) ]
                                [ EBin <cpt_Leeftijd ]
                          , Dnf [ ECps (EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                                       ,EDcD rel_leeftijd_Persoon_Leeftijd
                                       ) ]
                                [ ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                       ,EBin <cpt_Leeftijd
                                       ) ]
                          , Dnf [ ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ,ECps (EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                                             ,EDcD rel_leeftijd_Persoon_Leeftijd
                                             )
                                       ) ]
                                [ EBin <cpt_Leeftijd ]
                          ]
          }
   conj_3
    = Cjct{ rc_id         = "conj_3"
          , rc_orgRules   = [ rule_kindIsJonger4]
          , rcConjunct   = EUni (ECpl (EFlp (EDcD rel_isOuderVan_Persoon_Persoon))
                               ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                     ,ECps (EBin <=cpt_Leeftijd
                                           ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                           )
                                     )
                               )
          , rc_dnfClauses = [ Dnf [ EFlp (EDcD rel_isOuderVan_Persoon_Persoon) ]
                                [ ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                       ,ECps (EBin <=cpt_Leeftijd
                                             ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                             )
                                       ) ]
                          , Dnf [ ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ,EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                                       ) ]
                                [ ECps (EBin <=cpt_Leeftijd
                                       ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ) ]
                          , Dnf [ ECps (ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                             ,EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                                             )
                                       ,EDcD rel_leeftijd_Persoon_Leeftijd
                                       ) ]
                                [ EBin <=cpt_Leeftijd ]
                          , Dnf [ ECps (EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                                       ,EDcD rel_leeftijd_Persoon_Leeftijd
                                       ) ]
                                [ ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                       ,EBin <=cpt_Leeftijd
                                       ) ]
                          , Dnf [ ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ,ECps (EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                                             ,EDcD rel_leeftijd_Persoon_Leeftijd
                                             )
                                       ) ]
                                [ EBin <=cpt_Leeftijd ]
                          ]
          }
   conj_4
    = Cjct{ rc_id         = "conj_4"
          , rc_orgRules   = [ rule_ouderIsOuder1, rule_ouderIsOuder3]
          , rcConjunct   = EUni (ECpl (EDcD rel_isOuderVan_Persoon_Persoon)
                               ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                     ,ECps (EDcD rel_groterDan_Leeftijd_Leeftijd
                                           ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                           )
                                     )
                               )
          , rc_dnfClauses = [ Dnf [ EDcD rel_isOuderVan_Persoon_Persoon ]
                                [ ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                       ,ECps (EDcD rel_groterDan_Leeftijd_Leeftijd
                                             ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                             )
                                       ) ]
                          , Dnf [ ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ,EDcD rel_isOuderVan_Persoon_Persoon
                                       ) ]
                                [ ECps (EDcD rel_groterDan_Leeftijd_Leeftijd
                                       ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ) ]
                          , Dnf [ ECps (ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                             ,EDcD rel_isOuderVan_Persoon_Persoon
                                             )
                                       ,EDcD rel_leeftijd_Persoon_Leeftijd
                                       ) ]
                                [ EDcD rel_groterDan_Leeftijd_Leeftijd ]
                          , Dnf [ ECps (EDcD rel_isOuderVan_Persoon_Persoon
                                       ,EDcD rel_leeftijd_Persoon_Leeftijd
                                       ) ]
                                [ ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                       ,EDcD rel_groterDan_Leeftijd_Leeftijd
                                       ) ]
                          , Dnf [ ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ,ECps (EDcD rel_isOuderVan_Persoon_Persoon
                                             ,EDcD rel_leeftijd_Persoon_Leeftijd
                                             )
                                       ) ]
                                [ EDcD rel_groterDan_Leeftijd_Leeftijd ]
                          ]
          }
   conj_5
    = Cjct{ rc_id         = "conj_5"
          , rc_orgRules   = [ rule_ouderIsOuder2]
          , rcConjunct   = EUni (ECpl (EDcD rel_isOuderVan_Persoon_Persoon)
                               ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                     ,ECps (EBin >cpt_Leeftijd
                                           ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                           )
                                     )
                               )
          , rc_dnfClauses = [ Dnf [ EDcD rel_isOuderVan_Persoon_Persoon ]
                                [ ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                       ,ECps (EBin >cpt_Leeftijd
                                             ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                             )
                                       ) ]
                          , Dnf [ ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ,EDcD rel_isOuderVan_Persoon_Persoon
                                       ) ]
                                [ ECps (EBin >cpt_Leeftijd
                                       ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ) ]
                          , Dnf [ ECps (ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                             ,EDcD rel_isOuderVan_Persoon_Persoon
                                             )
                                       ,EDcD rel_leeftijd_Persoon_Leeftijd
                                       ) ]
                                [ EBin >cpt_Leeftijd ]
                          , Dnf [ ECps (EDcD rel_isOuderVan_Persoon_Persoon
                                       ,EDcD rel_leeftijd_Persoon_Leeftijd
                                       ) ]
                                [ ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                       ,EBin >cpt_Leeftijd
                                       ) ]
                          , Dnf [ ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ,ECps (EDcD rel_isOuderVan_Persoon_Persoon
                                             ,EDcD rel_leeftijd_Persoon_Leeftijd
                                             )
                                       ) ]
                                [ EBin >cpt_Leeftijd ]
                          ]
          }
   conj_6
    = Cjct{ rc_id         = "conj_6"
          , rc_orgRules   = [ rule_ouderIsOuder4]
          , rcConjunct   = EUni (ECpl (EDcD rel_isOuderVan_Persoon_Persoon)
                               ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                     ,ECps (EBin >=cpt_Leeftijd
                                           ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                           )
                                     )
                               )
          , rc_dnfClauses = [ Dnf [ EDcD rel_isOuderVan_Persoon_Persoon ]
                                [ ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                       ,ECps (EBin >=cpt_Leeftijd
                                             ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                             )
                                       ) ]
                          , Dnf [ ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ,EDcD rel_isOuderVan_Persoon_Persoon
                                       ) ]
                                [ ECps (EBin >=cpt_Leeftijd
                                       ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ) ]
                          , Dnf [ ECps (ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                             ,EDcD rel_isOuderVan_Persoon_Persoon
                                             )
                                       ,EDcD rel_leeftijd_Persoon_Leeftijd
                                       ) ]
                                [ EBin >=cpt_Leeftijd ]
                          , Dnf [ ECps (EDcD rel_isOuderVan_Persoon_Persoon
                                       ,EDcD rel_leeftijd_Persoon_Leeftijd
                                       ) ]
                                [ ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                       ,EBin >=cpt_Leeftijd
                                       ) ]
                          , Dnf [ ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ,ECps (EDcD rel_isOuderVan_Persoon_Persoon
                                             ,EDcD rel_leeftijd_Persoon_Leeftijd
                                             )
                                       ) ]
                                [ EBin >=cpt_Leeftijd ]
                          ]
          }
   conj_7
    = Cjct{ rc_id         = "conj_7"
          , rc_orgRules   = [ rule_TOT_3040012754054565971]
          , rcConjunct   = EUni (ECpl (EDcI cpt_Persoon)
                               ,ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                     ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                     )
                               )
          , rc_dnfClauses = [ Dnf [ EDcI cpt_Persoon ]
                                [ ECps (EDcD rel_leeftijd_Persoon_Leeftijd
                                       ,EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ) ]
                          , Dnf [ ECps (EFlp (EDcD rel_leeftijd_Persoon_Leeftijd)
                                       ,EDcD rel_leeftijd_Persoon_Leeftijd
                                       ) ]
                                [ EDcI cpt_Leeftijd ]
                          ]
          }

 -- *** Quads (total: 22 quads) ***: 
   quad_rel_leeftijd_Persoon_Leeftijd_TOT_3040012754054565971
    = Quad{ qDcl     = rel_leeftijd_Persoon_Leeftijd
          , qRule    = rule_TOT_3040012754054565971
          , qConjuncts = [ conj_7 ]
          }
   quad_rel_leeftijd_Persoon_Leeftijd_UNI_3040012754054565971
    = Quad{ qDcl     = rel_leeftijd_Persoon_Leeftijd
          , qRule    = rule_UNI_3040012754054565971
          , qConjuncts = [ conj_0 ]
          }
   quad_rel_groterDan_Leeftijd_Leeftijd_kindIsJonger1
    = Quad{ qDcl     = rel_groterDan_Leeftijd_Leeftijd
          , qRule    = rule_kindIsJonger1
          , qConjuncts = [ conj_1 ]
          }
   quad_rel_isOuderVan_Persoon_Persoon_kindIsJonger1
    = Quad{ qDcl     = rel_isOuderVan_Persoon_Persoon
          , qRule    = rule_kindIsJonger1
          , qConjuncts = [ conj_1 ]
          }
   quad_rel_leeftijd_Persoon_Leeftijd_kindIsJonger1
    = Quad{ qDcl     = rel_leeftijd_Persoon_Leeftijd
          , qRule    = rule_kindIsJonger1
          , qConjuncts = [ conj_1 ]
          }
   quad_rel_isOuderVan_Persoon_Persoon_kindIsJonger2
    = Quad{ qDcl     = rel_isOuderVan_Persoon_Persoon
          , qRule    = rule_kindIsJonger2
          , qConjuncts = [ conj_2 ]
          }
   quad_rel_leeftijd_Persoon_Leeftijd_kindIsJonger2
    = Quad{ qDcl     = rel_leeftijd_Persoon_Leeftijd
          , qRule    = rule_kindIsJonger2
          , qConjuncts = [ conj_2 ]
          }
   quad_rel_groterDan_Leeftijd_Leeftijd_kindIsJonger3
    = Quad{ qDcl     = rel_groterDan_Leeftijd_Leeftijd
          , qRule    = rule_kindIsJonger3
          , qConjuncts = [ conj_1 ]
          }
   quad_rel_isOuderVan_Persoon_Persoon_kindIsJonger3
    = Quad{ qDcl     = rel_isOuderVan_Persoon_Persoon
          , qRule    = rule_kindIsJonger3
          , qConjuncts = [ conj_1 ]
          }
   quad_rel_leeftijd_Persoon_Leeftijd_kindIsJonger3
    = Quad{ qDcl     = rel_leeftijd_Persoon_Leeftijd
          , qRule    = rule_kindIsJonger3
          , qConjuncts = [ conj_1 ]
          }
   quad_rel_isOuderVan_Persoon_Persoon_kindIsJonger4
    = Quad{ qDcl     = rel_isOuderVan_Persoon_Persoon
          , qRule    = rule_kindIsJonger4
          , qConjuncts = [ conj_3 ]
          }
   quad_rel_leeftijd_Persoon_Leeftijd_kindIsJonger4
    = Quad{ qDcl     = rel_leeftijd_Persoon_Leeftijd
          , qRule    = rule_kindIsJonger4
          , qConjuncts = [ conj_3 ]
          }
   quad_rel_groterDan_Leeftijd_Leeftijd_ouderIsOuder1
    = Quad{ qDcl     = rel_groterDan_Leeftijd_Leeftijd
          , qRule    = rule_ouderIsOuder1
          , qConjuncts = [ conj_4 ]
          }
   quad_rel_isOuderVan_Persoon_Persoon_ouderIsOuder1
    = Quad{ qDcl     = rel_isOuderVan_Persoon_Persoon
          , qRule    = rule_ouderIsOuder1
          , qConjuncts = [ conj_4 ]
          }
   quad_rel_leeftijd_Persoon_Leeftijd_ouderIsOuder1
    = Quad{ qDcl     = rel_leeftijd_Persoon_Leeftijd
          , qRule    = rule_ouderIsOuder1
          , qConjuncts = [ conj_4 ]
          }
   quad_rel_isOuderVan_Persoon_Persoon_ouderIsOuder2
    = Quad{ qDcl     = rel_isOuderVan_Persoon_Persoon
          , qRule    = rule_ouderIsOuder2
          , qConjuncts = [ conj_5 ]
          }
   quad_rel_leeftijd_Persoon_Leeftijd_ouderIsOuder2
    = Quad{ qDcl     = rel_leeftijd_Persoon_Leeftijd
          , qRule    = rule_ouderIsOuder2
          , qConjuncts = [ conj_5 ]
          }
   quad_rel_groterDan_Leeftijd_Leeftijd_ouderIsOuder3
    = Quad{ qDcl     = rel_groterDan_Leeftijd_Leeftijd
          , qRule    = rule_ouderIsOuder3
          , qConjuncts = [ conj_4 ]
          }
   quad_rel_isOuderVan_Persoon_Persoon_ouderIsOuder3
    = Quad{ qDcl     = rel_isOuderVan_Persoon_Persoon
          , qRule    = rule_ouderIsOuder3
          , qConjuncts = [ conj_4 ]
          }
   quad_rel_leeftijd_Persoon_Leeftijd_ouderIsOuder3
    = Quad{ qDcl     = rel_leeftijd_Persoon_Leeftijd
          , qRule    = rule_ouderIsOuder3
          , qConjuncts = [ conj_4 ]
          }
   quad_rel_isOuderVan_Persoon_Persoon_ouderIsOuder4
    = Quad{ qDcl     = rel_isOuderVan_Persoon_Persoon
          , qRule    = rule_ouderIsOuder4
          , qConjuncts = [ conj_6 ]
          }
   quad_rel_leeftijd_Persoon_Leeftijd_ouderIsOuder4
    = Quad{ qDcl     = rel_leeftijd_Persoon_Leeftijd
          , qRule    = rule_ouderIsOuder4
          , qConjuncts = [ conj_6 ]
          }

 -- *** PlugInfos (total: 5 plugInfos) ***: 
   plug_Leeftijd
    = let sqlAtt_Leeftijd
           = Att { attSQLColName    = Leeftijd
                 , attExpr    = EDcI cpt_Leeftijd
                 , attType    = INTEGER
                 , attUse     = PlainAttr 
                 , attNull    = False
                 , attDBNull  = False
                 , attUniq    = True
                 , attFlipped = False
                 }
      in
      TblSQL { sqlname    = Leeftijd
             , attributes = [sqlAtt_Leeftijd]
             , cLkpTbl    = [ (cpt_Leeftijd, sqlAtt_Leeftijd)]
             , dLkpTbl    = [ ]
             }
   plug_ONE
    = let sqlAtt_ONE
           = Att { attSQLColName    = ONE
                 , attExpr    = EDcI cptOne
                 , attType    = OBJECT
                 , attUse     = PrimaryKey cptOne
                 , attNull    = False
                 , attDBNull  = False
                 , attUniq    = True
                 , attFlipped = False
                 }
      in
      TblSQL { sqlname    = ONE
             , attributes = [sqlAtt_ONE]
             , cLkpTbl    = [ (cptOne, sqlAtt_ONE)]
             , dLkpTbl    = [ ]
             }
   plug_Persoon
    = let sqlAtt_Persoon
           = Att { attSQLColName    = Persoon
                 , attExpr    = EDcI cpt_Persoon
                 , attType    = OBJECT
                 , attUse     = PrimaryKey cpt_Persoon
                 , attNull    = False
                 , attDBNull  = False
                 , attUniq    = True
                 , attFlipped = False
                 }
          sqlAtt_leeftijd
           = Att { attSQLColName    = leeftijd
                 , attExpr    = EDcD rel_leeftijd_Persoon_Leeftijd
                 , attType    = INTEGER
                 , attUse     = ForeignKey cpt_Leeftijd
                 , attNull    = False
                 , attDBNull  = True
                 , attUniq    = False
                 , attFlipped = False
                 }
      in
      TblSQL { sqlname    = Persoon
             , attributes = [sqlAtt_Persoon, sqlAtt_leeftijd]
             , cLkpTbl    = [ (cpt_Persoon, sqlAtt_Persoon)]
             , dLkpTbl    = [ RelStore { rsDcl           = rel_leeftijd_Persoon_Leeftijd
                                       , rsStoredFlipped = False
                                       , rsSrcAtt        = sqlAtt_Persoon
                                       , rsTrgAtt        = sqlAtt_leeftijd
                                       }]
             }
   plug_groterDan
    = let sqlAtt_SrcLeeftijd
           = Att { attSQLColName    = SrcLeeftijd
                 , attExpr    = EIsc (EDcI cpt_Leeftijd
                                  ,ECps (EDcD rel_groterDan_Leeftijd_Leeftijd
                                        ,EFlp (EDcD rel_groterDan_Leeftijd_Leeftijd)
                                        )
                                  )
                 , attType    = INTEGER
                 , attUse     = ForeignKey cpt_Leeftijd
                 , attNull    = False
                 , attDBNull  = False
                 , attUniq    = False
                 , attFlipped = False
                 }
          sqlAtt_TgtLeeftijd
           = Att { attSQLColName    = TgtLeeftijd
                 , attExpr    = EDcD rel_groterDan_Leeftijd_Leeftijd
                 , attType    = INTEGER
                 , attUse     = ForeignKey cpt_Leeftijd
                 , attNull    = False
                 , attDBNull  = False
                 , attUniq    = False
                 , attFlipped = False
                 }
      in
      BinSQL { sqlname = groterDan
             , cLkpTbl = [ ]
             , dLkpTbl    = [ RelStore { rsDcl           = rel_groterDan_Leeftijd_Leeftijd
                                       , rsStoredFlipped = False
                                       , rsSrcAtt        = sqlAtt_SrcLeeftijd
                                       , rsTrgAtt        = sqlAtt_TgtLeeftijd
                                       }]
             }
   plug_isOuderVan
    = let sqlAtt_SrcPersoon
           = Att { attSQLColName    = SrcPersoon
                 , attExpr    = EIsc (EDcI cpt_Persoon
                                  ,ECps (EDcD rel_isOuderVan_Persoon_Persoon
                                        ,EFlp (EDcD rel_isOuderVan_Persoon_Persoon)
                                        )
                                  )
                 , attType    = OBJECT
                 , attUse     = ForeignKey cpt_Persoon
                 , attNull    = False
                 , attDBNull  = False
                 , attUniq    = False
                 , attFlipped = False
                 }
          sqlAtt_TgtPersoon
           = Att { attSQLColName    = TgtPersoon
                 , attExpr    = EDcD rel_isOuderVan_Persoon_Persoon
                 , attType    = OBJECT
                 , attUse     = ForeignKey cpt_Persoon
                 , attNull    = False
                 , attDBNull  = False
                 , attUniq    = False
                 , attFlipped = False
                 }
      in
      BinSQL { sqlname = isOuderVan
             , cLkpTbl = [ ]
             , dLkpTbl    = [ RelStore { rsDcl           = rel_isOuderVan_Persoon_Persoon
                                       , rsStoredFlipped = False
                                       , rsSrcAtt        = sqlAtt_SrcPersoon
                                       , rsTrgAtt        = sqlAtt_TgtPersoon
                                       }]
             }

 -- *** Concepts (total: 3 concepts) ***: 
   cptOne
    = ONE
      -- atoms: [ `1`]
   cpt_Leeftijd
    = PlainConcept Leeftijd
      -- atoms: [ `3`, `4`, `6`, `8`, `10`, `12`, `22`, `24`, `25`, `26`, `27`, `28`, `29`, `30`, `31`, `32`, `33`, `34`, `36`, `37`, `39`, `40`, `41`, `45`]
   cpt_Persoon
    = PlainConcept Persoon
      -- atoms: [ `Floris Smit`
      --        , `Wesley Mulder`
      --        , `Mila Brouwer`
      --        , `Daan Bos`
      --        , `Bart Jansen`
      --        , `Zoe Hoekstra`
      --        , `Amber de Vries`
      --        , `Sam de Groot`
      --        , `Xander van der Meer`
      --        , `Gijs Kuipers`
      --        , `Vera Smit`
      --        , `Romy Visser`
      --        , `Noah Koster`
      --        , `Oscar Mulder`
      --        , `Charlotte de Wit`
      --        , `Yara Bakker`
      --        , `Emma van Dijk`
      --        , `Tess Schouten`
      --        , `Puck van den Berg`
      --        , `Jasper Veenstra`
      --        , `Hanna Hendriks`
      --        , `Kim de Lange`
      --        , `Isa Meijer`
      --        , `Lars Maas`
      --        , `Quinten Roos`
      --        , `Ugo de Vries`]
