{-# OPTIONS_GHC -Wall #-}
module Database.Design.Ampersand.Misc.Explain
    ( string2Blocks
    , blocks2String
    , PandocFormat(..)
    )
where

import Text.Pandoc
import Data.List (isPrefixOf)
import Database.Design.Ampersand.Core.ParseTree      (PandocFormat(..))

-- | use a suitable format to read generated strings. if you have just normal text, ReST is fine.
-- | defaultPandocReader flags should be used on user-defined strings.
string2Blocks :: PandocFormat -> String -> [Block]
string2Blocks defaultformat str
 = case blocks of             -- WHY (SJ, dec 7th, 2011): What is the point of changing Para into Plain?
    [Para is] -> [Plain is]   -- BECAUSE (SJ, dec 7th, 2011): The table of relations in the LaTeX output of ChapterDataAnalysis gives errors when LaTeX is run, because Para generates a newline that LaTeX cannot cope with.
    _         -> blocks       --                              However, this Para is generated by Pandoc, so I'm wondering whether the mistake is in Pandoc? Anyway, this solution is a dirty fix, which I don't like.
   where
     Pandoc _ blocks = thePandocParser (removeCRs str')
     removeCRs :: String -> String
     removeCRs [] = []
     removeCRs ('\r' :'\n' : xs) = '\n' : removeCRs xs
     removeCRs (c:xs) = c:removeCRs xs
     (thePandocParser,str') = whatParser2UseOnWhatString
     whatParser2UseOnWhatString :: (String -> Pandoc,String)
     whatParser2UseOnWhatString -- = (readRST, str)
        | markDownPrefix `isPrefixOf` str = (readMarkdown def, drop (length markDownPrefix) str)
        | reSTPrefix     `isPrefixOf` str = (readRST      def, drop (length reSTPrefix)     str)
        | hTMLPrefix     `isPrefixOf` str = (readHtml     def, drop (length hTMLPrefix)     str)
         --stateParseRaw=True e.g. such that "\ref{something}" is not read as "\{something\}". with parse raw it's read as inline latex
         --maybe html should be parsed raw too...
        | laTeXPrefix    `isPrefixOf` str = (readLaTeX    def, drop (length laTeXPrefix)    str)
        | otherwise   = case defaultformat of
                               Markdown  -> (readMarkdown def, str)
                               ReST      -> (readRST      def, str)
                               HTML      -> (readHtml     def, str)
                               LaTeX     -> (readLaTeX    def, str)
       where markDownPrefix = makePrefix Markdown
             reSTPrefix     = makePrefix ReST
             hTMLPrefix     = makePrefix HTML
             laTeXPrefix    = makePrefix LaTeX

makePrefix :: PandocFormat -> String
makePrefix format = ":"++show format++":"

-- | write [Block] as String in a certain format using defaultWriterOptions
blocks2String :: PandocFormat -> Bool -> [Block] -> String
blocks2String format writeprefix ec
 = [c | c<-makePrefix format,writeprefix]
   --you cannot unwords lines for all writers, because white lines have a meaning in LaTeX i.e. \par
   --if your application of blocks2String may not have line breaks, then unwords lines there
   ++ {- unwords -} ( {-lines $ -} writer def (Pandoc nullMeta ec))
   where writer = case format of
            Markdown  -> writeMarkdown
            ReST      -> writeRST
            HTML      -> writeHtmlString
            LaTeX     -> writeLaTeX
