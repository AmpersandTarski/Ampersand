{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Misc.Explain
    ( string2Blocks
    , explainContent2String
    )
where

import Text.Pandoc
import DatabaseDesign.Ampersand.Misc.Options
import Data.List (isPrefixOf)
 
-- | use a suitable format to read generated strings. if you have just normal text, ReST is fine.
-- | defaultPandocReader flags should be used on user-defined strings.
string2Blocks :: PandocFormat -> String -> [Block]
string2Blocks defaultformat str = blocks
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
        | otherwise   = case defaultformat of
                          Markdown  -> (readMarkdown , str)
                          ReST      -> (readRST , str)
                          HTML      -> (readHtml , str)
                          LaTeX     -> (readLaTeX , str)
       where markDownPrefix = makePrefix Markdown
             reSTPrefix     = makePrefix ReST
             hTMLPrefix     = makePrefix HTML
             laTeXPrefix    = makePrefix LaTeX

makePrefix :: PandocFormat -> String             
makePrefix format = ":"++show format++":"

-- | write [Block] as String in a certain format using defaultWriterOptions
explainContent2String :: PandocFormat -> Bool -> [Block] -> String
explainContent2String format writeprefix ec 
 = [c | c<-makePrefix format,writeprefix]
   ++ unwords ( lines $ writer defaultWriterOptions (Pandoc (Meta [][][]) ec))
   where writer = case format of
            Markdown  -> writeMarkdown
            ReST      -> writeRST
            HTML      -> writeHtmlString
            LaTeX     -> writeLaTeX
