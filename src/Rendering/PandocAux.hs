module Rendering.PandocAux where
import Char                          
import Version          (versionbanner)
import Adl
import Picture
import ShowADL
import CommonClasses    (showSign)
import Data.Fspec
import Strings          (unCap, chain)
import Text.Pandoc  
  --Als de compiler hierover struikelt, dan moet je pandoc installeren. Dat is overigens in de volgende 3 stappen:
                          -- 1) Eerst installeer je Cabal (zie http://www.haskell.org/cabal/) en dan roep je op je command line: 
                          -- 2) cabal-install pandoc  (onder windows: cabal install pandoc)
                          -- 3) Het kan zijn dat dit nog niet werkt, zie http://groups.google.com/group/pandoc-discuss/browse_thread/thread/a8fc3a627aeec7f2
                          --    als dat het geval is, kan deze module worden overruled in Generators.hs    
                          -- Built on pandoc 1.4                             
import Languages        (Lang(..),plural)
import Options hiding (services) --importing (Options(..),FspecFormat(..))

-- TODO: Han, wil jij nog eens goed naar de PanDoc template kijken.
-- De onderstaande code is een vrij rauwe combinatie van de oude LaTeX header en het
-- default PanDoc template. Dat krijg je door op de command line   pandoc -D latex  uit te voeren.
-- In elk geval moeten de conditionals in LaTeX eruit en vervangen worden door Haskell conditionals.
-- Wellicht wordt e.e.a. daardoor simpeler.
laTeXtemplate :: Options->String
laTeXtemplate flags
   = chain "\n" (
     [ "% This header is the default LaTeX template for generating documents with Ampersand."
     , "% It was generated with "++versionbanner
     , "% You can modify this file to make it fit your needs. However, the required knowledge of "
     , "% LaTeX is not documented here. You can find help with that at http://en.wikibooks.org/wiki/LaTeX"
     , "% see the ampersand user guide (TODO) for more information on how to apply your own LaTeX header"
     , "%"
     , "\\documentclass[10pt,a4paper]{report}              % Define the document class"
     , "\\parskip 12pt plus 2.5pt minus 4pt                % Extra vertical space between paragraphs."
     , "\\parindent 0em                                    % Width of paragraph indentation."
     , ""
     , "% -- pachages used for several purposes:"
     , "\\usepackage{theorem}"
     , "\\usepackage{amssymb}"
     , "\\usepackage{amsmath}       % Provides various features to facilitate writing math formulas and to improve the typographical quality of their output."
  --   , "\\usepackage{hyperref}"
     ] ++
     ( case language flags of
        Dutch   -> [ "\\usepackage[dutch]{babel}"
                   , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{definition}{Definitie}[section]"
                   , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{designrule}[definition]{Functionele eis}" ]
        _       -> [ "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{definition}{Definition}[section]"
                   , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{designrule}[definition]{Requirement}" ]
     )++
     ["\\usepackage{graphicx}"                   | useGraphics flags] ++
--     ["\\graphicspath{{"++posixFilePath (dirOutput flags)++"}}" {- | graphics flags, equalFilePath (dirOutput flags) "." -}] ++  -- for multiple directories use \graphicspath{{images_folder/}{other_folder/}{third_folder/}}
     [ "\\def\\id#1{\\mbox{\\em #1\\/}}"
     , "\\newcommand{\\marge}[1]{\\marginpar{\\begin{minipage}[t]{3cm}{\\noindent\\small\\em #1}\\end{minipage}}}"
     , "\\def\\define#1{\\label{dfn:#1}\\index{#1}{\\em #1}}"
     , "\\def\\defmar#1{\\label{dfn:#1}\\index{#1}\\marge{#1}{\\em #1}}"
     , "\\newcommand{\\iden}{\\mathbb{I}}"
     , "\\newcommand{\\ident}[1]{\\mathbb{I}_{#1}}"
     , "\\newcommand{\\full}{\\mathbb{V}}"
     , "\\newcommand{\\fullt}[1]{\\mathbb{V}_{[#1]}}"
     , "\\newcommand{\\relAdd}{\\dagger}"
     , "\\newcommand{\\flip}[1]{{#1}^\\smallsmile} %formerly:  {#1}^\\backsim"
     , "\\newcommand{\\kleeneplus}[1]{{#1}^{+}}"
     , "\\newcommand{\\kleenestar}[1]{{#1}^{*}}"
     , "\\newcommand{\\cmpl}[1]{\\overline{#1}}"
     , "\\newcommand{\\rel}{\\times}"
     , "\\newcommand{\\compose}{;}"
     , "\\newcommand{\\subs}{\\vdash}"
     , "\\newcommand{\\fun}{\\rightarrow}"
     , "\\newcommand{\\isa}{\\sqsubseteq}"
     , "\\newcommand{\\N}{\\mbox{\\msb N}}"
     , "\\newcommand{\\disjn}[1]{\\id{disjoint}(#1)}"
     , "\\newcommand{\\fsignat}[3]{\\id{#1}:\\id{#2}\\mbox{\\(\\rightarrow\\)}\\id{#3}}"
     , "\\newcommand{\\signat}[3]{\\mbox{\\({#1}_{[{#2},{#3}]}\\)}}"
     , "\\newcommand{\\signt}[2]{\\mbox{\\({#1}_{[{#2}]}\\)}}"
     , "\\newcommand{\\declare}[3]{\\id{#1}:\\id{#2}\\mbox{\\(\\times\\)}\\id{#3}}"
     , "\\newcommand{\\fdeclare}[3]{\\id{#1}:\\id{#2}\\mbox{\\(\\fun\\)}\\id{#3}}"
     ] ++ (if language flags == Dutch then [ "\\selectlanguage{dutch}" ] else [] )++
     [ "%  -- end of ADL-specific header. The remainder is PanDoc-specific. run C:>pandoc -D latex  to see the default template."
{-TODO: disabled while running on icommas.ou.nl (uses MikTex 2.5 i.e. without xetex)
 -    , "$if(xetex)$"
     , "\\usepackage{ifxetex}"
     , "\\ifxetex"
     , "  \\usepackage{fontspec,xltxtra,xunicode}"
     , "  \\defaultfontfeatures{Mapping=tex-text,Scale=MatchLowercase}"
     , "\\else"
     , "  \\usepackage[mathletters]{ucs}"
     , "  \\usepackage[utf8x]{inputenc}"
     , "\\fi"
     , "$else$"
     , "\\usepackage[mathletters]{ucs}"
     , "\\usepackage[utf8x]{inputenc}"
     , "$endif$" -}
     , "$if(lhs)$"
     , "\\usepackage{listings}"
     , "\\lstnewenvironment{code}{\\lstset{language=Haskell,basicstyle=\\small\\ttfamily}}{}"
     , "$endif$"
     , "\\setlength{\\parindent}{0pt}"
     , "\\setlength{\\parskip}{6pt plus 2pt minus 1pt}"
     , "$if(verbatim-in-note)$"
     , "\\usepackage{fancyvrb}"
     , "$endif$"
     , "$if(fancy-enums)$"
     , "\\usepackage{enumerate}"
     , "$endif$"
     , "$if(tables)$"
     , "\\usepackage{array}"
     , "% This is needed because raggedright in table elements redefines \\\\:"
     , "\\newcommand{\\PreserveBackslash}[1]{\\let\\temp=\\\\#1\\let\\\\=\\temp}"
     , "\\let\\PBS=\\PreserveBackslash"
     , "$endif$"
     , "$if(strikeout)$"
     , "\\usepackage[normalem]{ulem}"
     , "$endif$"
     , "$if(subscript)$"
     , "\\newcommand{\\textsubscr}[1]{\\ensuremath{_{\\scriptsize\\textrm{#1}}}}"
     , "$endif$"
     , "$if(links)$"
     , "\\usepackage[breaklinks=true]{hyperref}"
     , "$endif$"
     , "$if(url)$"
     , "\\usepackage{url}"
     , "$endif$"
     , "$if(numbersections)$"
     , "$else$"
     , "\\setcounter{secnumdepth}{0}"
     , "$endif$"
     , "$if(verbatim-in-note)$"
     , "\\VerbatimFootnotes % allows verbatim text in footnotes"
     , "$endif$"
     , "$for(header-includes)$"
     , "$header-includes$"
     , "$endfor$"
     , ""
     , "$if(title)$"
     , "\\title{$title$}"
     , "$endif$"
     , "\\author{$for(author)$$author$$sep$\\\\$endfor$}"
     , "$if(date)$"
     , "\\date{$date$}"
     , "$endif$"
     , ""
     , "\\begin{document}"
     , "$if(title)$"
     , "\\maketitle"
     , "$endif$"
     , ""
     , "$if(toc)$"
     , "\\tableofcontents"
     , ""
     , "$endif$"
     , "$body$"
     , ""
     , "\\end{document}"
     ])

-----Linguistic goodies--------------------------------------

count :: Options -> Int -> String -> String
count flags n x
 = case (language flags, n) of
      (Dutch  , 0) -> "geen "++plural Dutch x
      (Dutch  , 1) -> preciesEen++" "++x                -- zou "één" moeten zijn, maar dit geeft een UTF-8 decoding error in de Haskell compiler (TODO).
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
    where
      preciesEen = "een (1)" --"één"  TODO moet nog utf8 resistent worden gemaakt.
    
------ Symbolic referencing ---------------------------------

class SymRef a where
  symLabel     :: a -> String -- unique label for symbolic reference purposes
  symReqLabel  :: a -> Inline  -- labels the requirement of a
  symReqLabel   c = TeX $ "\\label{Req"++symLabel c++"}"
  symDefLabel  :: a -> Inline  -- labels the definition of a
  symDefLabel   c = TeX $ "\\label{Def"++symLabel c++"}"
  symReqRef    :: a -> Inline  -- references the requirement of a
  symReqRef     c = TeX $ "\\ref{Req"++symLabel c++"}"
  symDefRef    :: a -> Inline  -- references the definition of a 
  symDefRef     c = TeX $ "\\ref{Def"++symLabel c++"}"
  symReqPageRef :: a -> Inline  -- references the requirement of a
  symReqPageRef c = TeX $ "\\pageref{Req"++symLabel c++"}"
  symDefPageRef :: a -> Inline  -- references the definition of a 
  symDefPageRef c = TeX $ "\\pageref{Def"++symLabel c++"}"

instance SymRef Concept where
  symLabel c = "Concept:"++latexEsc [x|x<-name c, x/='_']

instance SymRef Declaration where
  symLabel d = "Decl:"++latexEsc [x|x<-name d++name (source d)++name (target d), x/='_']

instance SymRef Rule where
  symLabel r = "Rule:"++latexEsc [x|x<-name r, x/='_']

--   xrefChptReference :: String -> [Inline]
--   xrefChptReference myLabel = [TeX ("\\ref{section:"++myLabel++"}")] --TODO werkt nog niet correct
---   xrefTableReference :: String -> [Inline]
--   xrefTableReference myLabel = [TeX ("\\ref{tab:"++myLabel++"}")]
-- DAAROM: Why (SJ) does the code of labeledHeader look stupid?
--         When Han looked at pandoc code TexInfo.hs blockToTexinfo ln.208,
--         he expected chapter,section,sub,subsub respectively.
--         However, he got section,sub,subsub,plain text respectively.
--         Looks like an error in PanDoc, doesn't it?
--         So now he wrote chapters as 0 setting a [Inline] -> [TeX "\\chapter{...}"].
--         We do not know yet how this relates to other formats like rtf.

labeledHeader :: Int -> String -> String -> [Block]
labeledHeader 0 lbl str =
                 [Para [TeX ("\\chapter{"++latexEsc str++"}"), xrefLabel lbl]]
labeledHeader lev lbl str =
                 [Header lev [Str str]]
              ++ [Para [xrefLabel lbl]]
 
xrefReference :: String -> Inline    -- uitbreidbaar voor andere rendering dan LaTeX
xrefReference myLabel = TeX ("\\ref{"++myLabel++"}")
xrefLabel :: String -> Inline        -- uitbreidbaar voor andere rendering dan LaTeX
xrefLabel myLabel = TeX ("\\label{"++myLabel++"}")
xrefCitation :: String -> Inline    -- uitbreidbaar voor andere rendering dan LaTeX
xrefCitation myLabel = TeX ("\\cite{"++myLabel++"}")

--Image [Inline] Target
--      alt.text (URL,title)
xrefFigure1 :: Picture -> [Inline]
xrefFigure1 pict = 
   [ TeX "\\begin{figure}[htb]\n\\begin{center}\n\\scalebox{.3}[.3]{"
   , Image [Str $ "Here, "++uniqueName pict++" should have been visible"] ((uniqueName pict), (figlabel pict))
   , TeX "}\n"
   , TeX ("\\caption{"++latexEsc (caption pict)++"}\n") 
   , xrefLabel (figlabel pict)
   , TeX "\n\\end{center}\n\\end{figure}"]

pandocEqnArray :: [([Inline],[Inline],[Inline])] -> [Block]
pandocEqnArray xs
 = [ Para ([ TeX "\\begin{eqnarray}\n   " ]++
           chain [LineBreak,Str "\n   "] [ a++[TeX "&"]++b++[TeX "&"]++c | (a,b,c)<-xs ]++
           [ TeX ("\n\\end{eqnarray}") ]
          )
   | not (null xs)]
   
pandocEquation :: [Inline] -> [Block]
pandocEquation x
 = [ Para ([ TeX "\\begin{equation}\n   " ]++
           x++
           [ TeX ("\n\\end{equation}") ]
          )
   | not (null x)]


--DESCR -> pandoc print functions for Adl data structures
---------------------------------------
-- LaTeX math markup
---------------------------------------

class ShowMath a where
 showMath :: a -> String
 showMathcode :: Fspc -> a -> String
 showMathcode _ x = showMath x

instance ShowMath Concept where
 showMath c = "\\id{"++latexEsc (name c)++"}"
 showMathcode _ c = "\\id{"++latexEsc (name c)++"}"

instance ShowMath Gen where
 showMath g = showMath (genspc g) ++"\\ \\le\\ "++showMath (gengen g)
 showMathcode fSpec g = showMathcode fSpec (genspc g) ++"\\ \\le\\ "++showMathcode fSpec (gengen g)

instance ShowMath Rule where
 showMath r = error ("!Fatal (module Fspec2Pandoc 889): Please supply specification of the context in showMath "++showADL r)
 showMathcode fSpec r
  = {- ( if isSignal r
      then "\\verb#RULE # \\id{"++name r++"}\\ \\verb# SIGNALS #"
      else "\\verb#RULE # \\id{"++name r++"}\\ \\verb# MAINTAINS #"
    )++  -}
    case rrsrt r of
      Truth          -> showMathcode fSpec (rrcon r)
      Implication    -> showMathcode fSpec (rrant r) ++"\\ \\subs\\ "++showMathcode fSpec (rrcon r)
      Equivalence    -> showMathcode fSpec (rrant r) ++"\\ =\\ " ++showMathcode fSpec (rrcon r)
      Generalization -> showMathcode fSpec (G (pos r) (source (rrcon r)) (source (rrant r)) "")

instance ShowMath Expression where
 showMath e           = (showchar.insParentheses) e
 showMathcode fSpec e = (showchar.insParentheses.disambiguate fSpec.mphatsoff) e

showchar :: Expression -> String
showchar (Tm mph _) = showMath mph
showchar (Fu [])  = "\\cmpl{\\full}"
showchar (Fu fs)  = chain "\\cup" [showchar f| f<-fs]     -- union
showchar (Fi [])  = "\\full"
showchar (Fi fs)  = chain "\\cap" [showchar f| f<-fs]     -- intersection
showchar (Fd [])  = "\\cmpl{\\iden}"
showchar (Fd ts)  = chain "\\relAdd" [showchar t| t<-ts]  -- relative addition (dagger)
showchar (F [])   = "\\iden"
showchar (F ts)   = chain "\\compose" [showchar t| t<-ts] -- relative multiplication (semicolon)
showchar (K0 e')  = "\\kleenestar{"++showchar e'++"}"
showchar (K1 e')  = "\\kleeneplus{"++showchar e'++"}"
showchar (Cp e')  = "\\cmpl{"++showchar e'++"}"
showchar (Tc f)   = "("++showchar f++")"

instance ShowMath Morphism where
 showMath mph@(Mph{})
  = if inline mph then mstr else "\\flip{"++mstr++"}"
    where
      mstr  = "\\id{"++latexEsc (name mph)++"}"++
              if null (mphats mph)
              then (if inline mph && mphtyp mph==(source s, target s) || not (inline mph) && mphtyp mph==(target s,source s) then "" else showSign [a,b])
              else showSign (mphats mph)
      s     = mphdcl mph
      (a,b) = mphtyp mph
 showMath m@(I{})
  = if null (mphats m) then "\\iden" else "\\ident{"++showSign (mphats m)++"}"
 showMath m@(V{})
  = if null (mphats m) then "\\full" else "\\fullt{"++showSign (mphats m)++"}"
 showMath m@(Mp1{})
  = "'"++mph1val m++"'"++(showSign [mph1typ m])


instance ShowMath Declaration where
 showMath decl@(Sgn{})
  = "\\declare{"++latexEsc (name decl)++"}{"++latexEsc (name (source decl))++"}{"++latexEsc (name (target decl))++"}"
 showMath Isn{}
  = "\\iden"
 showMath Vs{}
  = "\\full"
 showMath Iscompl{}
  = "\\cmpl{\\iden}"

latexEscShw :: (Show a) => a -> [Char]
latexEscShw x = latexEsc (show x)

latexEsc :: [Char] -> [Char]
latexEsc x
 = f x
   where f "" = ""
         f ('_':str) = "\\_"++f str
         f ('\192':str) = "\\`A" ++f str   -- À
         f ('\193':str) = "\\'A" ++f str   -- Á
         f ('\196':str) = "\\\"A"++f str   -- Ä
         f ('\200':str) = "\\`E" ++f str   -- È
         f ('\201':str) = "\\'E" ++f str   -- É
         f ('\203':str) = "\\\"E"++f str   -- Ë
         f ('\204':str) = "\\`I" ++f str   -- Ì
         f ('\205':str) = "\\'I" ++f str   -- Í
         f ('\207':str) = "\\\"I"++f str   -- Ï
         f ('\210':str) = "\\`O" ++f str   -- Ò
         f ('\211':str) = "\\'O" ++f str   -- Ó
         f ('\214':str) = "\\\"O"++f str   -- Ö
         f ('\217':str) = "\\`U" ++f str   -- Ù
         f ('\218':str) = "\\'U" ++f str   -- Ú
         f ('\220':str) = "\\\"U"++f str   -- Ü
         f ('\224':str) = "\\`a" ++f str   -- à
         f ('\225':str) = "\\'a" ++f str   -- á
         f ('\228':str) = "\\\"a"++f str   -- ä
         f ('\232':str) = "\\`e" ++f str   -- è
         f ('\233':str) = "\\'e" ++f str   -- é
         f ('\235':str) = "\\\"e"++f str   -- ë
         f ('\236':str) = "\\`i" ++f str   -- ì
         f ('\237':str) = "\\'i" ++f str   -- í
         f ('\239':str) = "\\\"i"++f str   -- ï
         f ('\242':str) = "\\`o" ++f str   -- ò
         f ('\243':str) = "\\'o" ++f str   -- ó
         f ('\246':str) = "\\\"o"++f str   -- ö
         f ('\249':str) = "\\`u" ++f str   -- ù
         f ('\250':str) = "\\'u" ++f str   -- ú
         f ('\252':str) = "\\\"u"++f str   -- ü
         f (c:str)   = c: f str

--posixFilePath :: FilePath -> String
-- tex uses posix file notation, however when on a windows machine, we have windows conventions for file paths...
-- To set the graphicspath, we want something like: \graphicspath{{"c:/data/ADL/output/"}}
--posixFilePath fp = "/"++System.FilePath.Posix.addTrailingPathSeparator (System.FilePath.Posix.joinPath   (tail  (splitDirectories fp)))

makeDefinition :: Options -> String -> String -> [Inline]
makeDefinition flags c cdef
  = case language flags of
     English -> [Str ("A"++(if unCap (take 1 c) `elem` ["a","e","i","o","u"] then "n" else "")++" ")]++str++[Str (" is "++cdef)]
     Dutch   -> [Str "Een "]++str++[Str (" is "++cdef)]
    where
     str = [Emph [Str (unCap c)]]++[TeX ("\\index{"++latexEsc (unCap c)++"}\\marge{"++latexEsc (unCap c)++"}") | fspecFormat flags==FLatex]
