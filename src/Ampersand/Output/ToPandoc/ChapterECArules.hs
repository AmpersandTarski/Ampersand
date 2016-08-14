{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.ToPandoc.ChapterECArules
where
import Ampersand.Output.ToPandoc.SharedAmongChapters
import Ampersand.FSpec.ToFSpec.NormalForms (proofPA)


chpECArules :: FSpec -> Blocks
chpECArules fSpec
 | (not.genEcaDoc.getOpts) fSpec = mempty
 | otherwise = 
     chptHeader (fsLang fSpec) EcaRules
  <> (para.str.l) (NL "Dit hoofdstuk bevat de ECA regels."
                  ,EN "This chapter lists the ECA rules.")
  <> para (  (str.l) (NL "ECA regels:"                 , EN "ECA rules:")
           <> linebreak
           <> (str.l) (NL"   tijdelijk ongedocumenteerd", EN "   temporarily not documented")
          )
  <> mconcat
       [   (blockQuote . codeBlock . ("\n     "++) . showECA "\n     ") eca
        <> if False -- TODO : This could be enabled by introducing a switch in Options.hs
           then mempty
           else    para (   linebreak
                         <> (str.l) (NL "------ Afleiding ----->"  ,EN "------ Derivation ----->")
                        )
                <> (showProof (codeBlock . ("\n     "++) . showECA "\n     ") . proofPA (getOpts fSpec) . ecaAction) eca   --  for the proof
                <> para (   linebreak
                         <> (str.l) (NL "<------Einde afleiding --",EN "<------End Derivation --") 
                        )
       | eca<-vEcas fSpec, (not.isNop.ecaAction) eca
       ]

 where
  -- shorthand for easy localizing    
  l :: LocalizedStr -> String
  l lstr = localize (fsLang fSpec) lstr

                         