{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Misc.Explain
    ( string2Blocks
    , explainContent2String   -- Currently used for error messages in Prototype and Atlas.
    )
where

import Text.Pandoc
import DatabaseDesign.Ampersand.Misc.Options
import Data.List (isPrefixOf)
 
string2Blocks :: Options -> String -> [Block]
string2Blocks flags str = blocks
   where 
     Pandoc _ blocks = thePandocParser defaultParserState (removeCRs str')
     removeCRs :: String -> String
     removeCRs [] = []
     removeCRs ('\r' :'\n' : xs) = '\n' : removeCRs xs
     removeCRs (c:xs) = c:removeCRs xs
     (thePandocParser,str') = whatParser2UseOnWhatString
     whatParser2UseOnWhatString :: (ParserState -> String -> Pandoc,String)
     whatParser2UseOnWhatString -- = (readRST, str)
        | markDownPrefix `isPrefixOf` str = (readMarkdown, drop (length markDownPrefix) str)
        | reSTPrefix `isPrefixOf` str     = (readRST     , drop (length reSTPrefix)     str)
        | hTMLPrefix `isPrefixOf` str     = (readHtml    , drop (length hTMLPrefix)     str)
        | laTeXPrefix `isPrefixOf` str    = (readLaTeX   , drop (length laTeXPrefix)    str)
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

--not all text can be parsed multiline (e.g. PHRASE)
explainContent2String :: [Block] -> String
explainContent2String ec = unwords ( lines $ writeRST defaultWriterOptions (Pandoc (Meta [][][]) ec))
     
