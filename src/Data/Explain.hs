{-# OPTIONS_GHC -Wall #-}
module Data.Explain (        ExplainContent,         AutoExplain(..)
                    , string2ExplainContent,  string2AutoExplain
                  --  , inlines2AutoExplain
                    , explainContent2String   -- Currently used for error messages in Prototype and Atlas.
                    , explainContent2Blocks
                    , explainParagraph
                    )
where

import Text.Pandoc
import Languages
import Options 

data AutoExplain = Because Lang ExplainContent deriving (Eq,Show)
 
type ExplainContent = PandocADL --deriving (Eq,Show) --TODO: Show moet nog worden weggewerkt. 
data PandocADL = PAdl [Block] deriving (Eq,Show)

string2AutoExplain :: Options -> String -> AutoExplain  --TODO alle aanroepen hiervan vervangen door mooie PANDOC [Inline].
string2AutoExplain flags x = Because (language flags) (string2ExplainContent flags x)
explainParagraph :: Options -> [Inline] -> AutoExplain
explainParagraph flags x = Because (language flags) (PAdl [Para x])

string2ExplainContent :: Options -> String -> ExplainContent
-- TODO: An attempt should be undertaken to parse the string to a [Inline]. 
-- If this succeeds, that [Inline] should be used. If it fails, the String itself should be used. 
-- The parsing should be done using Pandoc parsers. (http://hackage.haskell.org/packages/archive/pandoc/1.6/doc/html/src/Text-Pandoc-Readers-Markdown.html#readMarkdown

--string2ExplainContent text = inlines2ExplainContent  (map line2inline (lines text))
--   where line2inline str = Str str

string2ExplainContent flags text = parseToPAdl text  --TODO: Met de flags kunnen we wellicht verschillende pandoc parsers organiseren....
parseToPAdl :: String -> PandocADL
parseToPAdl s = case readRST defaultParserState s of
                     Pandoc _ xs  -> PAdl xs
                     _            -> PAdl [Para [Str s]] 
                        
explainContent2String :: ExplainContent -> String
explainContent2String ec = writeMarkdown defaultWriterOptions dummyPandoc
  where dummyPandoc = Pandoc (Meta [][][]) theBlock  --TODO Resultaat bekijken, eventueel moet er nog wat van de string worden gesloopt. 
        PAdl theBlock = ec

explainContent2Blocks :: ExplainContent -> [Block]
explainContent2Blocks (PAdl bs) = bs

