{-# OPTIONS_GHC -Wall #-}
module Data.Explain (        ExplainContent,         AutoExplain(..)
                    , string2ExplainContent,  string2AutoExplain
                    , inlines2AutoExplain
                    , explainContent2String
                    , explainContent2Inlines
                    )
where

import Text.Pandoc
import Languages

data AutoExplain = Because Lang ExplainContent deriving (Eq,Show)
 
data ExplainContent = Expl (Either String [Inline]) deriving (Eq,Show) --TODO: Show moet nog worden weggewerkt. 

string2AutoExplain :: Lang -> String -> AutoExplain  --TODO alle aanroepen hiervan vervangen door mooie PANDOC [Inline].
string2AutoExplain lang x = Because lang (string2ExplainContent x)
inlines2AutoExplain :: Lang -> [Inline] -> AutoExplain
inlines2AutoExplain lang x = Because lang (inlines2ExplainContent x)

string2ExplainContent :: String -> ExplainContent
-- TODO: An attempt should be undertaken to parse the string to a [Inline]. 
-- If this succeeds, that [Inline] should be used. If it fails, the String itself should be used. 
-- The parsing should be done using Pandoc parsers. (http://hackage.haskell.org/packages/archive/pandoc/1.6/doc/html/src/Text-Pandoc-Readers-Markdown.html#readMarkdown
string2ExplainContent text = inlines2ExplainContent  (map line2inline (lines text))
   where line2inline str = Str str
   
explainContent2String :: ExplainContent -> String
explainContent2String (Expl (Left x)) = x
explainContent2String (Expl (Right x)) =   
    case x of 
      [Str str] -> str
      _         -> error "!Fatal (module Data.Explain 28): Het zou te mooi zijn om hier uit te leggen..."  --TODO: prettyprint the pandoc structure.

explainContent2Inlines :: ExplainContent -> [Inline]
explainContent2Inlines (Expl (Right x)) = x 
explainContent2Inlines (Expl (Left str)) = [Str str]

inlines2ExplainContent :: [Inline] -> ExplainContent
inlines2ExplainContent x = Expl (Right x)