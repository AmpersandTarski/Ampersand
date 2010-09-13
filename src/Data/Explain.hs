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
 
type ExplainContent = [Block]

string2AutoExplain :: Options -> String -> AutoExplain  
string2AutoExplain flags x = Because (language flags) (rst2Blocks x)
explainParagraph :: Options -> [Inline] -> AutoExplain
explainParagraph flags x = Because (language flags) [Para x]

string2ExplainContent :: Options -> String -> ExplainContent
string2ExplainContent _ text = rst2Blocks text  --TODO: Met de flags kunnen we wellicht verschillende pandoc parsers organiseren....

rst2Blocks :: String -> [Block]
rst2Blocks str = blocks
   where 
     Pandoc _ blocks = readRST defaultParserState (removeLineFeeds str)
     removeLineFeeds :: String -> String
     removeLineFeeds [] = []
     removeLineFeeds ('\r' :'\n' : xs) = '\n' : (removeLineFeeds xs)
     removeLineFeeds (c:xs) = c:removeLineFeeds xs

explainContent2String :: ExplainContent -> String
explainContent2String ec = writeRST defaultWriterOptions (Pandoc (Meta [][][]) ec)

explainContent2Blocks :: ExplainContent -> [Block]
explainContent2Blocks ( bs) = bs

