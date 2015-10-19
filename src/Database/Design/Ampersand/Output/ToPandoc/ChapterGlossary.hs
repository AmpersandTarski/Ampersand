{-# LANGUAGE ScopedTypeVariables #-}
module Database.Design.Ampersand.Output.ToPandoc.ChapterGlossary
  (chpGlossary)
where
import Database.Design.Ampersand.Output.ToPandoc.SharedAmongChapters

chpGlossary :: Int -> FSpec ->  Blocks
chpGlossary _ fSpec
 = fromList $
   if fspecFormat (getOpts fSpec)==FLatex
   then [ Para [RawInline (Format "latex") "\\printglossaries"] ]
   else [ Table [] [AlignLeft,AlignLeft,AlignLeft] [0.0,0.0,0.0]
          ( case fsLang fSpec of
               Dutch   ->
                 [ [Plain [Str "term"]] , [Plain [Str "definitie"]] , [Plain [Str "bron"]]]
               English ->
                 [ [Plain [Str "term"]] , [Plain [Str "definition"]], [Plain [Str "source"]]]
          )
          [ [ [Plain [(Str . name)  c]], [Plain [(Str . cddef) cd]], [Plain [(Str . cdref) cd]]]
          | c<-concs fSpec, cd<-concDefs fSpec c
          ]]
