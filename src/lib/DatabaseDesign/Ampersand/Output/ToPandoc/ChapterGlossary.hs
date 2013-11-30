{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterGlossary
  (chpGlossary)
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes


chpGlossary :: Int -> Fspc -> Options ->  Blocks
chpGlossary _ fSpec flags
 = fromList $
   if fspecFormat flags==FLatex
   then [ Para [RawInline "latex" "\\printglossaries"] ]
   else [ Table [] [AlignLeft,AlignLeft,AlignLeft] [0.0,0.0,0.0]
          ( case language flags of
               Dutch   ->
                 [ [Plain [Str "term"]] , [Plain [Str "definitie"]] , [Plain [Str "bron"]]]
               English ->
                 [ [Plain [Str "term"]] , [Plain [Str "definition"]], [Plain [Str "source"]]]
          )
          [ [ [Plain [(Str . name)  c]], [Plain [(Str . cddef) cd]], [Plain [(Str . cdref) cd]]]
          | c<-concs fSpec, cd<-cptdf c
          ]]
