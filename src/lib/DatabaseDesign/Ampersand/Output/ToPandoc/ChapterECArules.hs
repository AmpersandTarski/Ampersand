{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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
   = case fsLang fSpec of
       Dutch   -> plain "Dit hoofdstuk bevat de ECA regels."
       English -> plain "This chapter lists the ECA rules."
     
  ifcECA :: Blocks
  ifcECA
   = case fsLang fSpec of
      Dutch   -> para ( "ECA regels:" <> linebreak 
                     <> "   tijdelijk ongedocumenteerd")
                     <> mconcat 
                         [   (blockQuote . codeBlock . ("\n     "++) . showECA "\n     ") eca
                          <> para ( linebreak <>
                                    "------ Afleiding ----->"   -- Dit in- en uitschakelbaar maken
                                  )
                          <> (showProof (codeBlock . ("\n     "++) . showECA "\n     ") . proofPA . ecaAction) eca   --  voor het bewijs
                          <> para ( linebreak <>
                                    "<------Einde afleiding --"   -- Dit in- en uitschakelbaar maken
                                  )
                         | eca<-vEcas fSpec, (not.isNop.ecaAction) eca
                         ]
      English -> para ( "ECA rules:" <> linebreak 
                     <> "   temporarily not documented")
                     <> mconcat 
                         [   (blockQuote . codeBlock . ("\n     "++) . showECA "\n     ") eca
                          <> para ( linebreak <>
                                    "------ Derivation ----->"   -- Dit in- en uitschakelbaar maken
                                  )
                          <> (showProof (codeBlock . ("\n     "++) . showECA "\n     ") . proofPA . ecaAction) eca   --  voor het bewijs
                          <> para ( linebreak <>
                                    "<------End Derivation --"   -- Dit in- en uitschakelbaar maken
                                  )
                         | eca<-vEcas fSpec, (not.isNop.ecaAction) eca
                         ]
