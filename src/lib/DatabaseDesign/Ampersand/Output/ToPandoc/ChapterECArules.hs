{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterECArules
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms (proofPA)
import DatabaseDesign.Ampersand.ADL1

chpECArules :: Fspc -> Options ->  Blocks
chpECArules fSpec _
 =   chptHeader (fsLang fSpec) EcaRules
  <> ecaIntro
  <> ifcECA
 where
  ecaIntro :: Blocks
  ecaIntro
   = fromList
     [ Plain $ case fsLang fSpec of
       Dutch   -> [Str "Dit hoofdstuk bevat de ECA regels." ]
       English -> [Str "This chapter lists the ECA rules." ]
     ]
  ifcECA :: Blocks
  ifcECA
   = fromList $
     case fsLang fSpec of
      Dutch   -> Para [ Str "ECA rules:",LineBreak, Str "   ",Str "tijdelijk ongedocumenteerd" ] : 
                 concat [ [ BlockQuote (toList (codeBlock ( showECA "\n     " eca )))
                          , Para [ LineBreak, Str "------ Afleiding ----->"] ]              ++   -- Dit in- en uitschakelbaar maken
                          toList (showProof (showECA "\n>     ") (proofPA (ecaAction eca))) ++   --  voor het bewijs
                          [ Para [ LineBreak, Str "<------Einde afleiding --"] ]
                        | eca<-vEcas fSpec, not (isNop (ecaAction eca))]
      English -> Para [ Str "ECA rules:",LineBreak, Str "   ",Str "temporarily not documented" ] :
                 concat [ [ BlockQuote (toList (codeBlock ( showECA "\n     " eca )))
                          , Para [ LineBreak, Str "------ Derivation ----->"] ]             ++   -- Dit in- en uitschakelbaar maken
                          toList (showProof (showECA "\n>     ") (proofPA (ecaAction eca))) ++   --  voor het bewijs
                          [ Para [ LineBreak, Str "<------End Derivation --"] ]
                        | eca<-vEcas fSpec, not (isNop (ecaAction eca))]
