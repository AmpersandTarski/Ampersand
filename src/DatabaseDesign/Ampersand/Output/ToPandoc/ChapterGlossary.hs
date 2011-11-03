{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.ChapterGlossary
where
import DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
import DatabaseDesign.Ampersand.Basics  
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import Data.List
import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms
import DatabaseDesign.Ampersand.Fspec.FPA (fpa) 
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Fspec.Fspec
import Text.Pandoc
import Text.Pandoc.Builder  (toList, codeBlock)
import DatabaseDesign.Ampersand.Output.PredLogic        (PredLogicShow(..), showLatex)
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec.Switchboard      (SwitchBdDiagram(..), switchboardAct,sbDiagram)
import DatabaseDesign.Ampersand.Output.AdlExplanation (purpose,meaning,Explainable(..))
import DatabaseDesign.Ampersand.Output.Statistics (Statistics(..))
import DatabaseDesign.Ampersand.Output.PandocAux


------------------------------------------------------------
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


--type Proof expr = [(expr,[String],String)]
--showProof :: (expr->String) -> Proof expr -> String
--showProof sh [(expr,_,_)]        = "\n      "++sh expr++"\n"
--showProof sh ((expr,ss,equ):prf) = "\n      "++sh expr++
--                                   "\n"++(if null ss then "\n   "++equ else if null equ then intercalate " " ss else "   "++equ++" { "++intercalate "; " ss++" }")++
--                                   showProof sh prf
--                                   --where e'= if null prf then "" else let (expr,_,_):_ = prf in showHS options "" expr 
--showProof _  []                  = ""
