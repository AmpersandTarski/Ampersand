{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterGlossary
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.Basics  
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Misc


glossary :: Int -> Fspc -> Options ->  [Block]
glossary _ fSpec flags
 = if fspecFormat flags==FLatex
   then [ Para [RawInline "latex" "\\printglossary"] ]
   else [ Table [] [AlignLeft,AlignLeft,AlignLeft] [0.0,0.0,0.0]
          ( case language flags of
               Dutch   ->
                 [ [Plain [Str "term"]] , [Plain [Str "definitie"]] , [Plain [Str "bron"]]]
               English ->
                 [ [Plain [Str "term"]] , [Plain [Str "definition"]], [Plain [Str "source"]]]
          )
          [ [ [Plain [(Str . name)  cd]], [Plain [(Str . cddef) cd]], [Plain [(Str . cdref) cd]]]
          | cd<-conceptDefs fSpec, name cd `elem` map name (concs fSpec)
          ]]
