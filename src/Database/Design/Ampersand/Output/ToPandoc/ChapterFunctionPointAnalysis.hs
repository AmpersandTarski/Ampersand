{-# LANGUAGE OverloadedStrings #-}
module Database.Design.Ampersand.Output.ToPandoc.ChapterFunctionPointAnalysis where

import Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters
import Database.Design.Ampersand.FSpec.FPA

-- TODO: add introductory and explanatory text to chapter
-- TODO: what about KGVs?

chpFunctionPointAnalysis :: FSpec -> Blocks
chpFunctionPointAnalysis fSpec
 =    chptHeader (fsLang fSpec) FunctionPointAnalysis
   <> para (   (str.l) (NL "Dit hoofdstuk ..."
                       ,EN "This chapter ...")
           )
   <> -- Data model section:
      table -- Caption: 
            ((str.l) (NL "Datamodel", EN "Data model"))
            -- Alignment:
            (replicate 4 (AlignLeft, 1/4))
            -- Header:
            (map (plain.str.l) 
               [ (NL "Type"        , EN "Type")
               , (NL "Naam"        , EN "Name")
               , (NL "Complexiteit", EN "Complexity")
               , (NL "FP"          , EN "FP")
               ])
            -- Data rows:
            ( [map (plain.str) 
                 [ l (NL "ILGV", EN "ILGV(???)")
                 , nm
                 , showLang (fsLang fSpec) cmplxty
                 , (show.fpVal) fp
                 ]
              | fp@FP{fpType=ILGV, fpName=nm, fpComplexity=cmplxty} <- fst (dataModelFPA fpa)
              ]++
              [ [ mempty
                , mempty
                , (plain.str.l) (NL "Totaal:", EN "Total:")
                , (plain.str.show.snd.dataModelFPA) fpa
              ] ]
            )
   <> -- User transaction section:
      table -- Caption
            ( (str.l)  (NL "Gebruikerstransacties", EN "User transactions"))
            -- Alignment:
            (replicate 4 (AlignLeft, 1/4))
            -- Header:
            (map (plain.str.l) 
               [ (NL "Type"        , EN "Type")
               , (NL "Naam"        , EN "Name")
               , (NL "Complexiteit", EN "Complexity")
               , (NL "FP"          , EN "FP")
               ])
            -- Data rows::
            ( [map plain 
                 [ (str.showLang (fsLang fSpec).fpType) fp
                 , (str.fpName) fp
                 , (str.showLang (fsLang fSpec).fpComplexity) fp
                 , (str.show.fpVal) fp
                 ]
              |fp@FP{} <- fst (userTransactionFPA fpa)
              ]++
              [ [ mempty
                , mempty
                , (plain.str.l) (NL "Totaal:", EN "Total:")
                , (plain.str.show.snd.userTransactionFPA) fpa
              ] ]
            )
       
   
  where
  -- shorthand for easy localizing    
  l :: LocalizedStr -> String
  l lstr = localize (fsLang fSpec) lstr

  fpa = fpAnalyze fSpec 

