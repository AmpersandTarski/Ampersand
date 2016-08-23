{-# LANGUAGE DeriveDataTypeable, CPP, MultiParamTypeClasses,
    FlexibleContexts, ScopedTypeVariables, PatternGuards,
    ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.PandocAux
      ( writepandoc
      , XRefObj(..) , xRefTo, xRefToLatexRefString
      , headerWithLabel
      , definitionListItemLabel
      , pandocEqnArray
      , pandocEqnArrayWithLabels
      , pandocEqnArrayWithLabel
      , pandocEquation
      , pandocEquationWithLabel
      , count
      , ShowMath(..)
      , latexEscShw, escapeNonAlphaNum
      , xrefCitation
      , texOnly_Id
      , texOnly_fun
      , texOnly_rel
      , newGlossaryEntry
      )
where
import Control.Monad
import Data.Char hiding    (Space)
import Data.List
import Data.Maybe
import Ampersand.ADL1
import Ampersand.Basics hiding (hPutStrLn)
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.FSpec
import Ampersand.Misc
import Ampersand.Prototype.StaticFiles_Generated
import Prelude hiding      (writeFile,readFile,getContents,putStr,putStrLn)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import System.Directory
import System.Environment
import qualified System.Exit as SE (ExitCode(..)) -- These are not considered Ampersand exit codes, but from Pandoc
import System.FilePath  -- (combine,addExtension,replaceExtension)
import System.IO (stderr, stdout)
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Process (pipeProcess)
#ifdef _WINDOWS
import Data.List (intercalate)
#endif

#ifdef _WINDOWS
changePathSeparators :: FilePath -> FilePath
changePathSeparators = intercalate "/" . splitDirectories
#endif


-- | Default key-value pairs for use with the Pandoc template
defaultWriterVariables :: FSpec -> [(String , String)]
defaultWriterVariables fSpec
  = [ ("title", (case (fsLang fSpec, diagnosisOnly (getOpts fSpec)) of
                        (Dutch  , False) -> if test (getOpts fSpec)
                                            then "Afspraken van "
                                            else "Functionele Specificatie van "
                        (English, False) -> "Functional Specification of "
                        (Dutch  ,  True) -> "Diagnose van "
                        (English,  True) -> "Diagnosis of "
                )++name fSpec)
 --   , ("mainfont",
 --   , ("sansfont",
 --   , ("monofont",
 --   , ("mathfont",
    , ("fontsize", "10pt")   --can be overridden by geometry package (see below)
    , ("lang"    , case fsLang fSpec of
                       Dutch   -> "dutch"
                       English -> "english")
    , ("papersize", "a4")
    , ("babel-lang", case fsLang fSpec of
                       Dutch   -> "dutch"
                       English -> "english")
    , ("documentclass","report")
    ] ++
    [ ("toc" , "<<TheTableOfContentsShouldGoHere>>") | not (diagnosisOnly (getOpts fSpec))]++
    [ ("header-includes", unlines
         [ "% ============Ampersand specific Begin================="
         , "% First a couple of LaTeX packages are included:"
         , ""
         , "% The glossaries package supports acronyms and multiple glossaries"
         , "\\usepackage[toc]{glossaries}    % Add the glossaries to the table of contents"
         , "\\makeglossaries"
         , ""
         , "% geometry provides a flexible and easy interface to page dimentions"
         , "\\usepackage[ top=1.5cm, bottom=1.5cm, outer=5cm, inner=2cm"
         , "            , heightrounded, footskip=.5cm"
         , "            , marginparwidth=2.5cm, marginparsep=0.5cm]{geometry}"
         , ""
         , "% breqn – Automatic line breaking of displayed equations"
         , "\\usepackage{breqn}"
         , ""
         , "% colonequals – Colon equals symbols"
         , "\\usepackage{colonequals}"
         , ""
         , ""
         , "\\usepackage{textcomp}"
         , "% == [all]{hypcap} after {hyperref} shows the ref'd picture i.o. the caption @ click =="
         , ""
         , "\\usepackage[all]{hypcap}"
         , ""

         , "% hack1) For the purpose of clear references in Latex. See also https://github.com/AmpersandTarski/ampersand/issues/31"
         , "\\makeatletter"
         , "\\let\\orgdescriptionlabel\\descriptionlabel"
         , "\\renewcommand*{\\descriptionlabel}[1]{%"
         , "  \\let\\orglabel\\label"
         , "  \\let\\label\\@gobble"
         , "  \\phantomsection"
         , "  \\edef\\@currentlabel{#1}%"
         , "  %\\edef\\@currentlabelname{#1}%"
         , "  \\let\\label\\orglabel"
         , "  \\orgdescriptionlabel{#1}%"
         , "}"
         , "\\makeatother"
         , "% End-hack1"
         , ""

         , "% hack2) The LaTeX commands \\[ and \\], are redefined in the amsmath package, making sure that equations are"
         , "% not numbered. This is undesireable behaviour. this is fixed with the following hack, inspired on a note"
         , "% found at http://tex.stackexchange.com/questions/40492/what-are-the-differences-between-align-equation-and-displaymath"
         , "\\DeclareRobustCommand{\\[}{\\begin{equation}}"
         , "\\DeclareRobustCommand{\\]}{\\end{equation}}"
         , "% End-hack2"
         , ""
         , ""
         , "\\def\\id#1{\\mbox{\\em #1\\/}}"
         , "\\newcommand{\\marge}[1]{\\marginpar{\\begin{minipage}[t]{3cm}{\\noindent\\small\\em #1}\\end{minipage}}}"
         , "\\def\\define#1{\\label{dfn:#1}\\index{#1}{\\em #1}}"
         , "\\def\\defmar#1{\\label{dfn:#1}\\index{#1}\\marge{#1}{\\em #1}}"
         , "\\newcommand{\\iden}{\\mathbb{I}}"
         , "\\newcommand{\\ident}[1]{\\mathbb{I}_{#1}}"
         , "\\newcommand{\\full}{\\mathbb{V}}"
         , "\\newcommand{\\fullt}[1]{\\mathbb{V}_{[#1]}}"
         , "\\newcommand{\\flip}[1]{{#1}^\\smallsmile} %formerly:  {#1}^\\backsim"
         , "%\\newcommand{\\kleeneplus}[1]{{#1}^{+}}"
         , "%\\newcommand{\\kleenestar}[1]{{#1}^{*}}"
         , "\\newcommand{\\asterisk}{*}"
         , "\\newcommand{\\cmpl}[1]{\\overline{#1}}"
         , "\\newcommand{\\subs}{\\vdash}"
         , "\\newcommand{\\rel}{\\times}"
         , "\\newcommand{\\fun}{\\rightarrow}"
         , "\\newcommand{\\isa}{\\sqsubseteq}"
         , "\\newcommand{\\N}{\\mbox{\\msb N}}"
         , "\\newcommand{\\disjn}[1]{\\id{disjoint}(#1)}"
         , "\\newcommand{\\fsignat}[3]{\\id{#1}:\\id{#2}\\fun\\id{#3}}"
         , "\\newcommand{\\signat}[3]{\\id{#1}:\\id{#2}\\rel\\id{#3}}"
         , "\\newcommand{\\signt}[2]{\\mbox{\\({#1}_{[{#2}]}\\)}}"
         , "\\newcommand{\\declare}[3]{\\id{#1}:\\ \\id{#2}\\rel\\id{#3}}"
         , "%\\newcommand{\\fdeclare}[3]{\\id{#1}:\\ \\id{#2}\\fun\\id{#3}}"
         , "% ============Ampersand specific End==================="
         ])
    | fspecFormat (getOpts fSpec) == FLatex ]

--DESCR -> functions to write the pandoc
--         String = the name of the outputfile
--         The IO() creates the actual output
writepandoc :: FSpec -> Pandoc -> IO()
writepandoc fSpec thePandoc = 
  do verboseLn (getOpts fSpec) ("Generating "++fSpecFormatString++" to : "++outputFile)
     writeFile outputFile (pandocWriter writerOptions thePandoc)
     
     case fspecFormat (getOpts fSpec) of
        FLatex  ->
           do result <- makePDF writeLaTeX writerOptions thePandoc fSpec
              case result of 
                Left err -> do putStrLn ("LaTeX Error: ")
                               B.putStr err
                               putStrLn "\n."
                Right _  -> do let pdfFile = outputFile -<.> "pdf"
                               verboseLn (getOpts fSpec) $ pdfFile ++" created."
             
        _ -> return ()
 where
    outputFile = 
      addExtension (dirOutput (getOpts fSpec) </> baseName (getOpts fSpec))
                   (case fspecFormat (getOpts fSpec) of
                      Fasciidoc     -> ".txt"
                      Fcontext      -> ".context"
                      Fdocbook      -> ".docbook"
                      Fman          -> ".man"
                      Fmarkdown     -> ".md"
                      Fmediawiki    -> ".mediawiki"
                      Forg          -> ".org"
                      Fplain        -> ".plain"
                      Frst          -> ".rst"
                      FPandoc       -> ".pandoc"
                      Frtf          -> ".rtf"
                      FLatex        -> ".ltx"
                      Fhtml         -> ".html"
                      Fopendocument -> ".odt"
                      Ftexinfo      -> ".texinfo"
                      Ftextile      -> ".textile"
                   )
    pandocWriter :: WriterOptions -> Pandoc -> String
    pandocWriter =
       case fspecFormat (getOpts fSpec) of
            Fasciidoc -> writeAsciiDoc
            FPandoc   -> writeNative
            Fcontext  -> writeConTeXt
            Fdocbook  -> writeDocbook
            Fhtml     -> writeHtmlString
            FLatex    -> writeLaTeX
            Fman      -> writeMan
            Fmarkdown -> writeMarkdown
            Fmediawiki -> writeMediaWiki
            Fopendocument -> writeOpenDocument
            Forg -> writeOrg
            Fplain -> writePlain
            Frst -> writeRST
            Frtf -> writeRTF
            Ftexinfo -> writeTexinfo
            Ftextile -> writeTextile
    fSpecFormatString :: String
    fSpecFormatString =
       case fspecFormat (getOpts fSpec) of
            FPandoc   -> "pandoc"
            Fasciidoc -> "asciidoc"
            Fcontext  -> "context"
            Fdocbook  -> "docbook"
            Fhtml     -> "html"
            FLatex    -> "latex"
            Fman      -> "man"
            Fmarkdown -> "markdown"
            Fmediawiki -> "mediawiki"
            Fopendocument -> "opendocument"
            Forg -> "org"
            Fplain -> "plain"
            Frst -> "rst"
            Frtf -> "rtf"
            Ftexinfo -> "texinfo"
            Ftextile -> "textile"
    writerOptions :: WriterOptions
    writerOptions = def
                      { writerStandalone=isJust template
                      , writerTableOfContents=True
                      , writerNumberSections=True
                      , writerTemplate=fromMaybe "" template
                      , writerVariables=defaultWriterVariables fSpec
                      , writerVerbose=verboseP (getOpts fSpec)
                      }
        where template = getStaticFileContent PandocTemplates ("default."++fSpecFormatString)


-----Linguistic goodies--------------------------------------

count :: Lang -> Int -> String -> String
count    lang    n      x
 = case (lang, n) of
      (Dutch  , 0) -> "geen "++plural Dutch x
      (Dutch  , 1) -> "één "++x
      (Dutch  , 2) -> "twee "++plural Dutch x
      (Dutch  , 3) -> "drie "++plural Dutch x
      (Dutch  , 4) -> "vier "++plural Dutch x
      (Dutch  , 5) -> "vijf "++plural Dutch x
      (Dutch  , 6) -> "zes "++plural Dutch x
      (Dutch  , _) -> show n++" "++plural Dutch x
      (English, 0) -> "no "++plural English x
      (English, 1) -> "one "++x
      (English, 2) -> "two "++plural English x
      (English, 3) -> "three "++plural English x
      (English, 4) -> "four "++plural English x
      (English, 5) -> "five "++plural English x
      (English, 6) -> "six "++plural English x
      (English, _) -> show n++" "++plural English x

------ Symbolic referencing ---------------------------------
data XRefObj = XRefNaturalLanguageDeclaration Declaration
             | XRefPredicateXpression Rule
             | XRefDataAnalRule Rule
             | XRefNaturalLanguageRule Rule
             | XRefProcessAnalysis Pattern
             | XRefProcessAnalysisDeclaration Declaration
             | XRefConceptualAnalysisPattern Pattern
             | XRefConceptualAnalysisDeclaration Declaration
             | XRefConceptualAnalysisRuleA Rule
             | XRefConceptualAnalysisRuleB Rule
             | XRefInterfacesInterface Interface
             | XRefNaturalLanguageTheme (Maybe Pattern)
xRefTo :: XRefObj -> Inlines
xRefTo x = rawInline "latex"  $ xRefToLatexRefString x
xRefToLatexRefString :: XRefObj -> String
xRefToLatexRefString x = "\\ref{"++xRefRawLabel x++"}"
xRefRawLabel :: XRefObj -> String
xRefRawLabel x
 = case x of
     XRefNaturalLanguageDeclaration d -> "natLangDcl:"++(escapeNonAlphaNum.fullName) d
     XRefPredicateXpression r     -> "pex:"++(escapeNonAlphaNum.name) r
     XRefDataAnalRule r           -> "dataAnalRule:"++(escapeNonAlphaNum.name) r
     XRefNaturalLanguageRule r    -> "natLangRule:"++(escapeNonAlphaNum.name) r
     XRefProcessAnalysis p        -> "prcAnal:"++(escapeNonAlphaNum.name) p
     XRefProcessAnalysisDeclaration d
                                  -> "prcAnalDcl:"++(escapeNonAlphaNum.fullName) d
     XRefConceptualAnalysisPattern p
                                  -> "cptAnalPat:"++(escapeNonAlphaNum.name) p
     XRefConceptualAnalysisDeclaration d
                                  -> "cptAnalDcl:"++(escapeNonAlphaNum.fullName) d
     XRefConceptualAnalysisRuleA r -> "cptAnalRuleA:"++(escapeNonAlphaNum.name) r
     XRefConceptualAnalysisRuleB r -> "cptAnalRuleB:"++(escapeNonAlphaNum.name) r
     XRefInterfacesInterface i    -> "interface:"++(escapeNonAlphaNum.name) i
     XRefNaturalLanguageTheme (Just t)
                                  -> "theme:"++(escapeNonAlphaNum.name) t
     XRefNaturalLanguageTheme Nothing
                                  -> ":losseEindjes"
    where
      fullName d = name d++"*"++(name.source) d++"*"++(name.target) d

headerWithLabel :: XRefObj -> Int -> Inlines -> Blocks
headerWithLabel x = headerWith (xRefRawLabel x, [],[])

definitionListItemLabel :: XRefObj -> String -> Inlines
definitionListItemLabel x prefix
  = str prefix <> rawInline "latex" ("\\label{"++xRefRawLabel x++"}")


xrefCitation :: String -> Inline    -- uitbreidbaar voor andere rendering dan LaTeX
xrefCitation myLabel = RawInline (Text.Pandoc.Builder.Format "latex") ("\\cite{"++escapeNonAlphaNum myLabel++"}")

pandocEqnArray :: [[String]] -> [Block]
pandocEqnArray [] = []
pandocEqnArray xs
 = (toList . para . displayMath)
       ( "\\begin{aligned}\n"
         ++ intercalate "\\\\\n   " [ intercalate "&" row  | row <-xs ]
         ++"\n\\end{aligned}"
       )

pandocEqnArrayWithLabels :: [(XRefObj,[String])] -> Blocks
pandocEqnArrayWithLabels [] = mempty
pandocEqnArrayWithLabels rows
 = (para .displayMath)
       ( "\\begin{aligned}\n"
         ++ intercalate "\\\\\n   " [ intercalate "&" row ++ "\\label{"++xRefRawLabel x++"}" | (x,row)<-rows ]
         ++"\n\\end{aligned}"
       )

pandocEqnArrayWithLabel :: XRefObj -> [[String]] -> Blocks
pandocEqnArrayWithLabel _ [] = mempty
pandocEqnArrayWithLabel xref rows
 = (para . displayMath)
       ( "\\label{"++xRefRawLabel xref++"}\\begin{aligned}\\\\\n"
         ++ intercalate "\\\\\n   " [ intercalate "&" row | row <- rows ]
         ++"\n\\end{aligned}"
       )

pandocEquation :: String -> [Block]
pandocEquation x = toList . para . displayMath $ x

pandocEquationWithLabel :: XRefObj -> String -> Blocks
pandocEquationWithLabel xref x =
   para . displayMath $
        ( "\\begin{aligned}\\label{"++xRefRawLabel xref++"}\\\\\n"
           ++x
           ++"\n\\end{aligned}"
        )


--DESCR -> pandoc print functions for Ampersand data structures
---------------------------------------
-- LaTeX math markup
---------------------------------------

class ShowMath a where
 showMath :: a -> String

instance ShowMath A_Concept where
 showMath c = texOnly_Id (name c)

instance ShowMath A_Gen where
 showMath g@Isa{} = showMath (genspc g)++"\\ \\le\\ "++showMath (gengen g)
 showMath g@IsE{} = showMath (genspc g)++"\\ =\\ "++intercalate "\\cap" (map showMath (genrhs g))

instance ShowMath Rule where
 showMath r = showMath (rrexp r)

instance ShowMath Signature where
 showMath (Sign s t) = showMath s++"\\rel"++showMath t

instance ShowMath Expression where
 showMath = showExpr . insParentheses
   where  showExpr (EEqu (l,r)) = showExpr l++texOnly_equals++showExpr r
          showExpr (EInc (l,r)) = showExpr l++texOnly_subs++showExpr r
          showExpr (EIsc (l,r)) = showExpr l++texOnly_inter++showExpr r
          showExpr (EUni (l,r)) = showExpr l++texOnly_union++showExpr r
          showExpr (EDif (l,r)) = showExpr l++texOnly_bx ++showExpr r
          showExpr (ELrs (l,r)) = showExpr l++texOnly_lRes++showExpr r
          showExpr (ERrs (l,r)) = showExpr l++texOnly_rRes++showExpr r
          showExpr (EDia (l,r)) = showExpr l++texOnly_dia++showExpr r
          showExpr (ECps (EEps i sgn,r)) | i==source sgn||i==target sgn = showExpr  r
                                         | otherwise                    = showExpr (ECps (EDcI i,r))
          showExpr (ECps (l,EEps i sgn)) | i==source sgn||i==target sgn = showExpr  l
                                         | otherwise                    = showExpr (ECps (l,EDcI i))
          showExpr (ECps (l,r)) = showExpr l++texOnly_compose++showExpr r
          showExpr (ERad (l,r)) = showExpr l++texOnly_relAdd++showExpr r
          showExpr (EPrd (l,r)) = showExpr l++texOnly_crtPrd++showExpr r
          showExpr (EKl0 e)     = showExpr (addParensToSuper e)++"^{"++texOnly_star++"}"
          showExpr (EKl1 e)     = showExpr (addParensToSuper e)++"^{"++texOnly_plus++"}"
          showExpr (EFlp e)     = showExpr (addParensToSuper e)++"^{"++texOnly_flip++"}"
          showExpr (ECpl e)     = "\\cmpl{"++showExpr e++"}"
          showExpr (EBrk e)     = "("++showExpr e++")"
          showExpr (EDcD d)     = "\\text{"++latexEscShw (name d)++"}"
          showExpr (EDcI c)     = "I_{[\\text{"++latexEscShw (name c)++"}]}"
          showExpr  EEps{}      = "" -- fatal 417 "EEps may occur only in combination with composition (semicolon)."  -- SJ 2014-03-11: Are we sure about this? Let's see if it ever occurs...
          showExpr (EDcV sgn)   = "V_{[\\text{"++latexEscShw (name (source sgn))++"}"++"*"
                                   ++"\\text{"++latexEscShw (name (target sgn))++"}]}"
          showExpr (EMp1 val _) = "`\\text{"++(latexEscShw . showADL $ val)++"}`"

-- add extra parentheses to consecutive superscripts, since latex cannot handle these
-- (this is not implemented in insParentheses because it is a latex-specific issue)
addParensToSuper :: Expression -> Expression
addParensToSuper e@EKl0{} = EBrk e
addParensToSuper e@EKl1{} = EBrk e
addParensToSuper e@EFlp{} = EBrk e
addParensToSuper e        = e

instance ShowMath Declaration where
 showMath decl@(Sgn{})
  = "\\declare{"++latexEscShw(name decl)++"}{"++latexEscShw(name (source decl))++"}{"++latexEscShw(name (target decl))++"}"
 showMath Isn{}
  = "\\mathbb{I}"
 showMath Vs{}
  = "\\full"

-- | latexEscShw escapes to LaTeX encoding. It is intended to be used in LaTeX text mode.
--   For more elaborate info on LaTeX encoding, consult the The Comprehensive LATEX Symbol List
--   on:    http://ftp.snt.utwente.nl/pub/software/tex/info/symbols/comprehensive/symbols-a4.pdf
latexEscShw :: String -> String
latexEscShw ""           = ""
latexEscShw ('\"':c:cs) | isAlphaNum c = "``"++latexEscShw (c:cs)
                        | otherwise    = "''"++latexEscShw (c:cs)
latexEscShw "\""        = "''"
latexEscShw (c:cs)      | isAlphaNum c && isAscii c = c:latexEscShw cs
                        | otherwise    = f c++latexEscShw cs
 where
  f '"' = "\\textquotedbl "
  f '#' = "\\#"
  f '$' = "\\$"
  f '%' = "\\%"
  f '&' = "\\&"
  f '\\'= "\\textbackslash "
  f '^' = "\\^{}"
  f '_' = "\\_"
  f '{' = "\\{"
  f '|' = "\\textbar "
  f '}' = "\\}"
  f '~' = "\\~{}"
  f '¦' = "\\textbrokenbar "
  f '¨' = "\\textasciidieresis "
  f '¯' = "\\textasciimacron "
  f '´' = "\\textasciiacute "
  f '¢' = "\\textcent "
  f '£' = "\\textpound "
  f '¤' = "\\textcurrency "
  f '¥' = "\\textyen "
  f '€' = "\\texteuro "
  f '<' = "\\textless "
  f '>' = "\\textgreater "
  f '±' = "\\textpm "
  f '«' = "\\guillemotleft "
  f '»' = "\\guillemotright "
  f '×' = "\\texttimes "
  f '÷' = "\\textdiv "
  f '§' = "\\S "
  f '©' = "\\textcopyright "
  f '¬' = "\\textlnot "
  f '®' = "\\textregistered "
  f '°' = "\\textdegree "
  f 'µ' = "\\textmu "
  f '¶' = "\\P "
  f '·' = "\\textperiodcentered "
  f '¼' = "\\textonequarter "
  f '½' = "\\textonehalf "
  f '¾' = "\\textthreequarters "
  f '¹' = "\\textonesuperior "
  f '²' = "\\texttwosuperior "
  f '³' = "\\textthreesuperior "
  f '∞' = "\\hbipropto "
  f 'ä' = "\\\"{a}"        --  umlaut or dieresis
  f 'Ä' = "\\\"{A}"        --  umlaut or dieresis
  f 'â' = "\\^{a}"         --  circumflex
  f 'Â' = "\\^{A}"         --  circumflex
  f 'à' = "\\`{a}"         --  grave accent
  f 'À' = "\\`{A}"         --  grave accent
  f 'á' = "\\'{a}"         --  acute accent
  f 'Á' = "\\'{A}"         --  acute accent
  f 'ã' = "\\~{a}"         --  tilde
  f 'Ã' = "\\~{A}"         --  tilde
  f 'å' = "\\aa "
--  f 'å' = "\\r{a}"       --  alternatively: ring over the letter
  f 'Å' = "\\AA "
--  f 'Å' = "\\r{A}"       --  alternatively: ring over the letter
  f 'ą' = "\\k{a}"         --  ogonek
  f 'Ą' = "\\k{A}"         --  ogonek
  f 'ª' = "\\textordfeminine "
  f 'æ' = "\\ae "
  f 'Æ' = "\\AE "
  f 'ç' = "\\c{c}"         --  cedilla
  f 'Ç' = "\\c{C}"         --  cedilla
  f 'Ð' = "\\DH "
  f 'ð' = "\\dh "
  f 'ë' = "\\\"{e}"        --  umlaut or dieresis
  f 'Ë' = "\\\"{E}"        --  umlaut or dieresis
  f 'ê' = "\\^{e}"         --  circumflex
  f 'Ê' = "\\^{E}"         --  circumflex
  f 'è' = "\\`{e}"         --  grave accent
  f 'È' = "\\`{E}"         --  grave accent
  f 'é' = "\\'{e}"         --  acute accent
  f 'É' = "\\'{E}"         --  acute accent
  f 'ï' = "\\\"{\\i}"      --  umlaut or dieresis
  f 'Ï' = "\\\"{I}"        --  umlaut or dieresis
  f 'î' = "\\^{\\i}"       --  circumflex
  f 'Î' = "\\^{I}"         --  circumflex
  f 'ì' = "\\`{\\i}"       --  grave accent
  f 'Ì' = "\\`{I}"         --  grave accent
  f 'í' = "\\'{\\i}"       --  acute accent
  f 'Í' = "\\'{I}"         --  acute accent
  f 'ł' = "\\l "           --  l with stroke
  f 'Ł' = "\\L "           --  l with stroke
  f 'n' = "\\~{n}"         --  tilde
  f 'Ñ' = "\\~{N}"         --  tilde
  f 'Ȯ' = "\\.{O}"         --  dot over the letter
  f 'ȯ' = "\\.{o}"         --  dot over the letter
  f 'ö' = "\\\"{o}"        --  umlaut or dieresis
  f 'Ö' = "\\\"{O}"        --  umlaut or dieresis
  f 'ô' = "\\^{o}"         --  circumflex
  f 'Ô' = "\\^{O}"         --  circumflex
  f 'ò' = "\\`{o}"         --  grave accent
  f 'Ò' = "\\`{O}"         --  grave accent
  f 'ó' = "\\'{o}"         --  acute accent
  f 'Ó' = "\\'{O}"         --  acute accent
  f 'õ' = "\\~{o}"         --  tilde
  f 'Õ' = "\\~{O}"         --  tilde
  f 'ō' = "\\={o}"         --  macron accent a bar over the letter)
  f 'Ō' = "\\={O}"         --  macron accent a bar over the letter)
  f 'ő' = "\\H{o}"         --  long Hungarian umlaut double acute)
  f 'Ő' = "\\H{O}"         --  long Hungarian umlaut double acute)
  f 'Ø' = "\\O "
  f 'ø' = "\\o "
  f 'º' = "\\textordmasculine "
  f 'ŏ' = "\\u{o}"         --  breve over the letter
  f 'Ŏ' = "\\u{O}"         --  breve over the letter
  f 'œ' = "\\oe "
  f 'Œ' = "\\OE "
  f 'š' = "\\v{s}"         --  caron/hacek "v") over the letter
  f 'Š' = "\\v{S}"         --  caron/hacek "v") over the letter
  f 'ß' = "\\ss "
  f 'Þ' = "\\TH "
  f 'þ' = "\\th "
  f '™' = "\\texttrademark "
  f 'ü' = "\\\"{u}"        --  umlaut or dieresis
  f 'Ü' = "\\\"{U}"        --  umlaut or dieresis
  f 'û' = "\\^{u}"         --  circumflex
  f 'Û' = "\\^{U}"         --  circumflex
  f 'ù' = "\\`{u}"         --  grave accent
  f 'Ù' = "\\`{U}"         --  grave accent
  f 'ú' = "\\'{u}"         --  acute accent
  f 'Ú' = "\\'{U}"         --  acute accent
  f 'ý' = "\\'{y}"         --  acute accent
  f 'Ý' = "\\'{Y}"         --  acute accent
  f _   = [c] -- let us think if this should be:    fatal 661 ("Symbol "++show x++" (character "++show (ord c)++") is not supported")

--posixFilePath :: FilePath -> String
-- tex uses posix file notation, however when on a windows machine, we have windows conventions for file paths...
-- To set the graphicspath, we want something like: \graphicspath{{"c:/data/Ampersand/output/"}}
--posixFilePath fp = "/"++System.FilePath.Posix.addTrailingPathSeparator (System.FilePath.Posix.joinPath   (tail  (splitDirectories fp)))


---------------------------
--- LaTeX related stuff ---
---------------------------
-- safe function to have plain text in a piece of Math
mathText :: String -> String
mathText s = "\\text{"++latexEscShw s++"} "

texOnly_Id :: String -> String
texOnly_Id s = "\\id{"++latexEscShw s++"} "

texOnly_fun :: String
texOnly_fun = "\\rightarrow "

texOnly_rel :: String
texOnly_rel = "\\times "

texOnly_compose :: String
texOnly_compose = ";"

texOnly_relAdd :: String
texOnly_relAdd = "\\dagger "

texOnly_crtPrd :: String
texOnly_crtPrd = "\\asterisk "

texOnly_inter :: String
texOnly_inter = "\\cap "

texOnly_union :: String
texOnly_union = "\\cup "

texOnly_subs :: String
texOnly_subs = "\\vdash "

texOnly_equals :: String
texOnly_equals = "="

texOnly_star :: String
texOnly_star = "^* "

texOnly_plus :: String
texOnly_plus = "^+ "

texOnly_bx :: String
texOnly_bx = " - "

texOnly_lRes :: String
texOnly_lRes = " / "

texOnly_rRes :: String
texOnly_rRes = " \\backslash "

texOnly_dia :: String
texOnly_dia = " \\Diamond "

texOnly_flip :: String
texOnly_flip = "\\smallsmile "

newGlossaryEntry :: String -> String -> Inlines
newGlossaryEntry nm cnt =
  rawInline "latex"
    ("\\newglossaryentry{"++escapeNonAlphaNum nm ++"}\n"++
     "     { name={"++latexEscShw nm ++"}\n"++
     "     , description={"++latexEscShw (cnt)++"}}\n")



-------------------------------------------------
---temporary from Pandoc:
------------------------------------------

makePDF :: (WriterOptions -> Pandoc -> String)  -- ^ writer
        -> WriterOptions       -- ^ options
        -> Pandoc              -- ^ document
        -> FSpec
        -> IO (Either B.ByteString B.ByteString)
makePDF writer wOpts pandoc fSpec = do
  tex2pdf' (dirOutput (getOpts fSpec))
  
  where 
    wVerbose = writerVerbose wOpts
    program  = "pdflatex" 
    args     = writerLaTeXArgs wOpts
    wSource  = writer wOpts pandoc
    tex2pdf' :: FilePath                        -- ^ temp directory for output
             -> IO (Either B.ByteString B.ByteString)
    tex2pdf' tmpDir = do
      let numruns = if "\\tableofcontents" `isInfixOf` wSource
                       then 3  -- to get page numbers
                       else 2  -- 1 run won't give you PDF bookmarks
      (exit, log', mbPdf) <- runTeXProgram 1 numruns tmpDir
      case (exit, mbPdf) of
           (SE.ExitFailure _, _)      -> do
              let logmsg = extractMsg log'
              return $ Left logmsg
           (SE.ExitSuccess, Nothing)  -> return $ Left ""
           (SE.ExitSuccess, Just pdf) -> return $ Right pdf

-- running tex programs

-- Run a TeX program on an input bytestring and return (exit code,
-- contents of stdout, contents of produced PDF if any).  Rerun
-- a fixed number of times to resolve references.
    runTeXProgram :: Int -> Int -> FilePath
                  -> IO (SE.ExitCode, B.ByteString, Maybe B.ByteString)
    runTeXProgram runNumber numRuns tmpDir = do
        let file = dirOutput (getOpts fSpec) </> baseName (getOpts fSpec) -<.> ".ltx"
        exists <- doesFileExist file
        unless exists $ fatal 766 $ "File should be written by now:\n  "++file 
#ifdef _WINDOWS
        -- note:  we want / even on Windows, for TexLive
        let tmpDir' = changePathSeparators tmpDir
        let file' = changePathSeparators file
#else
        let tmpDir' = tmpDir
        let file' = file
#endif
        let programArgs = ["-halt-on-error", "-interaction", "nonstopmode",
             "-output-directory", tmpDir'] ++ args ++ [file']
        env' <- getEnvironment
        let sep = [searchPathSeparator]
        let texinputs = maybe (tmpDir' ++ sep) ((tmpDir' ++ sep) ++)
              $ lookup "TEXINPUTS" env'
        let env'' = ("TEXINPUTS", texinputs) :
                      [(k,v) | (k,v) <- env', k /= "TEXINPUTS"]
        when (wVerbose && runNumber == 1) $ do
          putStrLn "[makePDF] temp dir:"
          putStrLn tmpDir'
          putStrLn "[makePDF] Command line:"
          putStrLn $ program ++ " " ++ unwords (map show programArgs)
          putStr "\n"
          putStrLn $ "[makePDF] Processing: " ++ file'
          putStr "\n"
        (exit, out, err) <- pipeProcess (Just env'') program programArgs BL.empty
        when wVerbose $ do
          putStrLn $ "[makePDF] Run #" ++ show runNumber
          B.hPutStr stdout out
          B.hPutStr stderr err
          putStr "\n"
        if runNumber <= numRuns
           then runTeXProgram (runNumber + 1) numRuns tmpDir
           else do
             let pdfFile = replaceDirectory (replaceExtension file ".pdf") tmpDir
             pdfExists <- doesFileExist pdfFile
             pdf <- if pdfExists
                       -- We read PDF as a strict bytestring to make sure that the
                       -- temp directory is removed on Windows.
                       -- See https://github.com/jgm/pandoc/issues/1192.
                       then (Just . B.fromChunks . (:[])) `fmap` BS.readFile pdfFile
                       else return Nothing
             return (exit, out <> err, pdf)

-- parsing output

extractMsg :: B.ByteString -> B.ByteString
extractMsg log' = do
  let msg'  = dropWhile (not . ("!" `BC.isPrefixOf`)) $ BC.lines log'
  let (msg'',rest) = break ("l." `BC.isPrefixOf`) msg'
  let lineno = take 1 rest
  if null msg'
     then log'
     else BC.unlines (msg'' ++ lineno)



