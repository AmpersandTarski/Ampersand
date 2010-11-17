{-# OPTIONS_GHC -Wall #-}
module Data.Explain ( ExplainContent,         AutoExplain(..)
                    , string2ExplainContent,  string2AutoExplain
                    , explainContent2String   -- Currently used for error messages in Prototype and Atlas.
                    , explainContent2Blocks
                    , explainParagraph
                    )
where

import Text.Pandoc
import Languages
import Options 
import Data.List (isPrefixOf)
data AutoExplain = Because Lang ExplainContent deriving (Eq,Show)
 
type ExplainContent = [Block]

string2AutoExplain :: Options -> String -> AutoExplain  
string2AutoExplain flags x = Because (language flags) (string2Blocks flags x)
explainParagraph :: Options -> [Inline] -> AutoExplain
explainParagraph flags x = Because (language flags) [Para x]

string2ExplainContent :: Options -> String -> ExplainContent
string2ExplainContent flags text = string2Blocks flags text  --TODO: Met de flags kunnen we wellicht verschillende pandoc parsers organiseren....

string2Blocks :: Options -> String -> [Block]
string2Blocks flags str = blocks
   where 
     Pandoc _ blocks = thePandocParser defaultParserState (removeCRs str')
     removeCRs :: String -> String
     removeCRs [] = []
     removeCRs ('\r' :'\n' : xs) = '\n' : (removeCRs xs)
     removeCRs (c:xs) = c:removeCRs xs
     (thePandocParser,str') = whatParser2UseOnWhatString
     whatParser2UseOnWhatString :: ((ParserState -> String -> Pandoc),String)
     whatParser2UseOnWhatString -- = (readRST, str)
        | isPrefixOf markDownPrefix str = (readMarkdown, drop (length markDownPrefix) str)
        | isPrefixOf reSTPrefix str     = (readRST     , drop (length reSTPrefix)     str)
        | isPrefixOf hTMLPrefix str     = (readHtml    , drop (length hTMLPrefix)     str)
        | isPrefixOf laTeXPrefix str    = (readLaTeX   , drop (length laTeXPrefix)    str)
        | otherwise   = case defaultPandocReader flags of
                          Markdown  -> (readMarkdown , str)
                          ReST      -> (readRST , str)
                          HTML      -> (readHtml , str)
                          LaTeX     -> (readLaTeX , str)
       where markDownPrefix = makePrefix Markdown
             reSTPrefix     = makePrefix ReST
             hTMLPrefix     = makePrefix HTML
             laTeXPrefix    = makePrefix LaTeX
             makePrefix pr = ":"++show pr++":"

explainContent2String :: ExplainContent -> String
explainContent2String ec = writeRST defaultWriterOptions (Pandoc (Meta [][][]) ec)

explainContent2Blocks :: ExplainContent -> [Block]
explainContent2Blocks ( bs) = bs


