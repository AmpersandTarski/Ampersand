{-# OPTIONS_GHC -Wall #-}
module Data.Explain (        ExplainContent,         AutoExplain(..)
                    , string2ExplainContent,  string2AutoExplain
             --       ,  block2ExplainContent,   block2AutoExplain
                    , explainContent2String
                    )
where

import Text.Pandoc
import Languages

data AutoExplain = Because Lang ExplainContent deriving (Eq,Show)
 
data ExplainContent = Expl (Either String Block) deriving (Eq,Show) --TODO: Show moet nog worden weggewerkt. 

string2AutoExplain :: Lang -> String -> AutoExplain  --TODO alle aanroepen hiervan vervangen door mooie PANDOC [Inline].
string2AutoExplain lang x = Because lang (string2ExplainContent x)

string2ExplainContent :: String -> ExplainContent
-- TODO: An attempt should be undertaken to parse the string to a Block. 
-- If this succeeds, that block should be used. If it fails, the String itself should be used. 
-- The parsing should be done using Pandoc parsers. (http://hackage.haskell.org/packages/archive/pandoc/1.6/doc/html/src/Text-Pandoc-Readers-Markdown.html#readMarkdown
string2ExplainContent x = Expl (Left x)
explainContent2String :: ExplainContent -> String
explainContent2String (Expl (Left x)) = x

block2AutoExplain :: Lang -> Block -> AutoExplain
block2AutoExplain lang x = Because lang (block2ExplainContent x)

block2ExplainContent :: Block -> ExplainContent
block2ExplainContent x = Expl (Right x)