{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterECArules
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Output.PandocAux

chpECArules :: Int -> Fspc -> Options ->  [Block]
chpECArules lev fSpec flags
 = header ++ ecaIntro ++ ifcECA
 where
  header :: [Block]
  header = labeledHeader lev "chpECA" "ECA rules"
  ecaIntro :: [Block]
  ecaIntro
   = [ Plain $ case language flags of
       Dutch   -> [Str "Dit hoofdstuk bevat de ECA regels." ]
       English -> [Str "This chapter lists the ECA rules." ]
     ]
  ifcECA :: [Block]
  ifcECA
   = case language flags of
      Dutch   -> Para [ Str "ECA rules:",LineBreak, Str "   ",Str "tijdelijk ongedocumenteerd" ] : 
                 [ BlockQuote (toList (codeBlock
                      ( showECA fSpec "\n     " eca
-- Dit inschakelen          ++[LineBreak, Str "------ Derivation ----->"]
--  voor het bewijs         ++(showProof (showECA fSpec [LineBreak, Str ">     ") (proofPA (ecaAction (eca arg)))
--                          ++[LineBreak, Str "<------End Derivation --"]
                      ) ) )
                 | eca<-vEcas fSpec, not (isNop (ecaAction eca))]
      English -> Para [ Str "ECA rules:",LineBreak, Str "   ",Str "temporarily not documented" ] :
                 [ BlockQuote (toList (codeBlock
                    ( showECA fSpec "\n" eca )))
                 | eca<-vEcas fSpec, not (isNop (ecaAction eca))]

