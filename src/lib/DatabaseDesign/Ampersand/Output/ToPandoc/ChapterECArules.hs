{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterECArules
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
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
                 [ BlockQuote (toList (codeBlock
                      ( showECA fSpec "\n     " eca
-- Dit inschakelen          ++[LineBreak, Str "------ Derivation ----->"]
--  voor het bewijs         ++showProof (showECA fSpec [LineBreak, Str ">     "]) (proofPA (ecaAction eca))
--                          ++[LineBreak, Str "<------End Derivation --"]
                      ) ) )
                 | eca<-vEcas fSpec, not (isNop (ecaAction eca))]
      English -> Para [ Str "ECA rules:",LineBreak, Str "   ",Str "temporarily not documented" ] :
                 [ BlockQuote (toList (codeBlock
                    ( showECA fSpec "\n" eca )))
                 | eca<-vEcas fSpec, not (isNop (ecaAction eca))]

