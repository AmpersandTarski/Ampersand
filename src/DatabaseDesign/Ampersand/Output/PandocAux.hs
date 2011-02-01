{-# OPTIONS_GHC -Wall -XFlexibleInstances #-}
module DatabaseDesign.Ampersand.Output.PandocAux
      ( writepandoc
      , labeledHeader
      , xrefReference
      , symDefLabel, symDefRef
      , symReqLabel, symReqRef, symReqPageRef
      , makeDefinition
      , xrefFigure1
      , pandocEqnArray
      , pandocEquation
      , count
      , showMathcode
      , latexEscShw
      , xrefCitation
      , texOnly_Id
      , texOnly_fun
      , texOnly_rel
      )
where
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Fspec.ShowADL(disambiguate,relatsoff)  --TODO: Should be removed over here.
import Data.Char
import Text.Pandoc
import DatabaseDesign.Ampersand.Basics  
import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)
import DatabaseDesign.Ampersand.Misc        
import System                 (system, ExitCode(ExitSuccess,ExitFailure))
--import System.Process
import System.FilePath        (combine,replaceExtension)
import System.Directory
import System.Info (os)
import Data.List              (isInfixOf,intercalate)
import Control.Monad
import Maybe                  (fromJust)

--DESCR -> functions to write the pandoc
--         String = the name of the outputfile
--         The first IO() is a Pandoc output format
--         The second IO(): If the output format is latex, then this IO() generates a .pdf from the .tex
writepandoc :: Options -> Pandoc -> (String,IO(),IO())
writepandoc flags thePandoc = (outputFile,makeOutput,postProcessMonad)
         where
         outputFile = replaceExtension (combine (dirOutput flags) (baseName flags)) 
                                       (case fspecFormat flags of        
                                                 FPandoc       -> ".pandoc"
                                                 FRtf          -> ".rtf"
                                                 FLatex        -> ".tex"
                                                 FHtml         -> ".html"
                                                 FOpenDocument -> ".odt"
                                       )
         makeOutput
          =  case fspecFormat flags of
              FPandoc -> do verboseLn flags ("Generating to Pandoc: "++outputFile)
                            writeFile outputFile (writeNative defaultWriterOptions  thePandoc)
              FRtf    -> do verboseLn flags ("Generating to Rich Text Format: "++outputFile)
                            writeFile outputFile (writeRTF ourDefaultWriterOptions{writerTemplate=theTemplate flags} thePandoc)
              FLatex  -> do --REMARK -> notice usage of fromJust
                            exists <- case texHdrFile flags of
                                         Just x -> doesFileExist x
                                         Nothing -> return False
                            header <- if exists 
                                      then readFile (fromJust$texHdrFile flags)
                                      else return (theTemplate flags)
                            verboseLn flags ("Generating to LaTeX: "++outputFile)
                            writeFile outputFile (writeLaTeX ourDefaultWriterOptions{writerTemplate=header} thePandoc)
              FHtml   -> do verboseLn flags ("Generating to HTML: "++outputFile)
                            writeFile outputFile (writeHtmlString  ourDefaultWriterOptions thePandoc)
              FOpenDocument 
                      -> do verboseLn flags ("Generating to Open Document Format: "++outputFile)
                            writeFile outputFile (writeOpenDocument ourDefaultWriterOptions thePandoc)
           where 
              ourDefaultWriterOptions = case theme flags of
                          ProofTheme -> defaultWriterOptions
                                          { writerStandalone=True }
                          _          -> defaultWriterOptions
                                          { writerStandalone=True
                                          , writerTableOfContents=True
                                          , writerNumberSections=True
                                          }
         postProcessMonad :: IO()
         postProcessMonad = 
           case fspecFormat flags of   
               FLatex  -> do 
                          (ready,nrOfRounds) <- doRestOfPdfLatex (False, 0)  -- initialize with: (<NotReady>, <0 rounds so far>)
                          verboseLn flags ("PdfLatex was called "++
                                           (if nrOfRounds>1 then show nrOfRounds++" times" else "once")++
                                           case ready of
                                              True  -> "."
                                              False -> ", but did not solve all references!")                          
                             where 
                                doRestOfPdfLatex :: (Bool,Int) -> IO (Bool,Int)
                                doRestOfPdfLatex (ready, roundsSoFar)
                                  = if or [ready, roundsSoFar > 4]    -- Make sure we will not hit a loop when something is wrong with call to pdfLatex ...
                                    then return (ready, roundsSoFar)
                                    else do callPdfLatexOnce
                                            let needle = "Rerun to get cross-references right." -- This is the text of the LaTeX Warning telling that label(s) may have changed. 
                                            {- The log file should be renamed before reading, because readFile opens the file
                                               for lazy IO. In a next run, pdfLatex will try to write to the log file again. If it
                                               was read using readFile, it will fail because the file is still open. 8-((  
                                            -} 
                                            renameFile (replaceExtension outputFile "log") (replaceExtension outputFile ("log"++show roundsSoFar))
                                            haystack <- readFile (replaceExtension outputFile ("log"++show roundsSoFar))  
                                            let notReady = isInfixOf needle haystack
                                            when notReady (verboseLn flags "Another round of pdfLatex is required. Hang on...")
                                          --  when notReady (dump "log")  -- Need to dump the last log file, otherwise pdfLatex cannot write its log.
                                            doRestOfPdfLatex (not notReady, roundsSoFar +1)
                  
                                callPdfLatexOnce :: IO ()
                                callPdfLatexOnce = 
                                   do result <- if os=="mingw32" || os=="mingw64" || os=="cygwin" || os=="windows" --REMARK: not a clear enum to check for windows OS
                                                then system ("pdflatex "++pdfflags++ outputFile++[x|x<-"> "++combine (dirOutput flags) "pdflog",not(verboseP flags)])  
                                                --REMARK: MikTex is windows; Tex-live does not have the flag -include-directory.
                                                else system ("cd "++(dirOutput flags)
                                                           ++" && pdflatex "
                                                           ++ replaceExtension (baseName flags) ".tex"
                                                           ++ [x|x<-"> pdflog",not(verboseP flags)])
                                      case result of 
                                         ExitSuccess   -> verboseLn flags ("PDF file created.")
                                         ExitFailure x -> verboseLn flags ("Failure: " ++ show x)
                                      where
                                      pdfflags = " -include-directory="++(dirOutput flags)++ " -output-directory="++(dirOutput flags)++" "
               _  -> return()            
               
-- TODO: Han, wil jij nog eens goed naar de PanDoc template kijken.
-- De onderstaande code is een vrij rauwe combinatie van de oude LaTeX header en het
-- default PanDoc template. Dat krijg je door op de command line   pandoc -D latex  uit te voeren.
-- In elk geval moeten de conditionals in LaTeX eruit en vervangen worden door Haskell conditionals.
-- Wellicht wordt e.e.a. daardoor simpeler.
theTemplate :: Options -> String
theTemplate flags 
  = case fspecFormat flags of
    FLatex -> intercalate "\n" (
               [ "% This header is the default LaTeX template for generating documents with Ampersand."
               , "% It was generated with "++ampersandCoreVersionBanner
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
               , "\\usepackage{amsmath}         % Provides various features to facilitate writing math formulas and to improve the typographical quality of their output."
            --   , "\\usepackage{hyperref}"
               , "\\usepackage{ucs}             % Provides various features for UTF8 (internationalization) stuff"
               , "\\usepackage[utf8x]{inputenc} %"
               ] ++
               ( case theme flags of
                  ProofTheme -> [ "\\usepackage[landscape]{geometry}"
                                , "%http://www.phil.cam.ac.uk/teaching_staff/Smith/logicmatters/l4llogiciansnew.html"
                                , "%http://www.phil.cam.ac.uk/teaching_staff/Smith/LaTeX/guides/BussGuide2.pdf"
                                , "\\usepackage{bussproofs}"
                                , "\\def\\defaultHypSeparation{\\hskip.0in}"
                                , "\\def\\ScoreOverhang{1pt}"]
                  _ -> []
               )++
               ( case language flags of
                  Dutch   -> [ "\\usepackage[dutch]{babel}"
                             , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{definition}{Definitie}[section]"
                             , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{designrule}[definition]{Functionele eis}" ]
                  English -> [ "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{definition}{Definition}[section]"
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
           --    , "\\newcommand{\\relAdd}{\\dagger}"
               , "\\newcommand{\\flip}[1]{{#1}^\\smallsmile} %formerly:  {#1}^\\backsim"
               , "\\newcommand{\\kleeneplus}[1]{{#1}^{+}}"
               , "\\newcommand{\\kleenestar}[1]{{#1}^{*}}"
               , "\\newcommand{\\cmpl}[1]{\\overline{#1}}"
           --    , "\\newcommand{\\rel}{\\times}"
           --    , "\\newcommand{\\compose}{;}"
               , "\\newcommand{\\subs}{\\vdash}"
           --    , "\\newcommand{\\fun}{\\rightarrow}"
               , "\\newcommand{\\isa}{\\sqsubseteq}"
               , "\\newcommand{\\N}{\\mbox{\\msb N}}"
               , "\\newcommand{\\disjn}[1]{"++texOnly_Id("disjoint")++"(#1)}"
               , "\\newcommand{\\fsignat}[3]{"++texOnly_Id("#1")++":"++texOnly_Id("#2")++"\\mbox{\\("++texOnly_fun++"\\)}"++texOnly_Id("#3")++"}"
               , "\\newcommand{\\signat}[3]{\\mbox{\\({#1}_{[{#2},{#3}]}\\)}}"
               , "\\newcommand{\\signt}[2]{\\mbox{\\({#1}_{[{#2}]}\\)}}"
               , "\\newcommand{\\declare}[3]{"++texOnly_Id("#1")++":"++texOnly_Id("#2")++"\\mbox{\\("++texOnly_rel++"\\)}"++texOnly_Id("#3")++"}"
               , "\\newcommand{\\fdeclare}[3]{"++texOnly_Id("#1")++":"++texOnly_Id("#2")++"\\mbox{\\("++texOnly_fun++"\\)}"++texOnly_Id("#3")++"}"
               ] ++ (if language flags == Dutch then [ "\\selectlanguage{dutch}" ] else [] )++
               [ "% ====================================================================================="
               , "% == The hyperref package will take care of turning all internal references of your  =="
               , "% == document into hyperlinks. For this to work properly some magic is necessary,    =="
               , "% == so you have to put \\usepackage[pdftex]{hyperref} as the last command            =="
               , "% == into the preamble of your document.                                             =="
               , "% == Many options are available to customize the behaviour of the hyperref           =="
               , "% == package. See http://ctan.tug.org/tex-archive/info/lshort/english/lshort.pdf     =="
               , "% ====================================================================================="
               , "\\usepackage[pdftex,colorlinks=false]{hyperref}"
               ] ++
               [ "%  -- end of Ampersand specific header. The remainder is PanDoc-specific. run C:>pandoc -D latex  to see the default template."
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


    FRtf -> intercalate "\n" (
               [ "$if(legacy-header)$"
               , "$legacy-header$"
               , "$else$"
               , "{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 \\fswiss Helvetica;}{\\f1 Courier;}}"
               , "{\\colortbl;\\red255\\green0\\blue0;\\red0\\green0\\blue255;}"
               , "\\widowctrl\\hyphauto"
               , "$endif$"
               , "$for(header-includes)$"
               , "$header-includes$"
               , "$endfor$"
               , ""
               , "$if(title)$"
               , "{\\pard \\qc \\f0 \\sa180 \\li0 \\fi0 \\b \\fs36 $title$\\par}"
               , "$endif$"
               , "$for(author)$"
               , "$endfor$"
               , "$if(date)$"
               , "{\\pard \\qc \\f0 \\sa180 \\li0 \\fi0  $date$\\par}"
               , "$endif$"
               , "$if(spacer)$"
               , "{\\pard \\ql \\f0 \\sa180 \\li0 \\fi0 \\par}"
               , "$endif$"
               , "$for(include-before)$"
               , "$include-before$"
               , "$endfor$"
               , "$body$"
               , "$for(include-after)$"
               , "$include-after$"
               , "$endfor$"
               , "}"
               ])
    FPandoc       -> error ("!Fatal (module Rendering.PandocAux 330): No template defined for Pandoc output") 
    FOpenDocument -> error ("!Fatal (module Rendering.PandocAux 331): No template defined for ODF output") 
    FHtml         -> error ("!Fatal (module Rendering.PandocAux 332): No template defined for HTML output") 
 
-----Linguistic goodies--------------------------------------

count :: Options -> Int -> String -> String
count flags n x
 = case (language flags, n) of
      (Dutch  , 0) -> "geen "++plural Dutch x
      (Dutch  , 1) -> preciesEen++" "++x
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
  symLabel c = "Concept:"++stripSpecialChars (name c)

instance SymRef (Declaration Concept) where
  symLabel d = "Decl:"++stripSpecialChars (name d++name (source d)++name (target d))

instance SymRef (Rule (Relation Concept)) where
  symLabel r = "Rule:"++stripSpecialChars (name r)

--   xrefChptReference :: String -> [Inline]
--   xrefChptReference myLabel = [TeX ("\\ref{section:"++myLabel++"}")] --TODO werkt nog niet correct
---   xrefTableReference :: String -> [Inline]
--   xrefTableReference myLabel = [TeX ("\\ref{tab:"++myLabel++"}")]
-- BECAUSE: Why (SJ) does the code of labeledHeader look stupid?
--         When Han looked at pandoc code TexInfo.hs blockToTexinfo ln.208,
--         he expected chapter,section,sub,subsub respectively.
--         However, he got section,sub,subsub,plain text respectively.
--         Looks like an error in PanDoc, doesn't it?
--         So now he wrote chapters as 0 setting a [Inline] -> [TeX "\\chapter{...}"].
--         We do not know yet how this relates to other formats like rtf.

labeledHeader :: Int -> String -> String -> [Block]
labeledHeader 0 lbl str =
                 [Para [TeX ("\\chapter{"++str++"}"), xrefLabel lbl]]
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
   , TeX ("\\caption{"++caption pict++"}\n") 
   , xrefLabel (figlabel pict)
   , TeX "\n\\end{center}\n\\end{figure}"]

pandocEqnArray :: [([Inline],[Inline],[Inline])] -> [Block]
pandocEqnArray xs
 = [ Para ([ TeX "\\begin{eqnarray}\n   " ]++
           intercalate [LineBreak,Str "\n   "] [ a++[TeX "&"]++b++[TeX "&"]++c | (a,b,c)<-xs ]++
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


--DESCR -> pandoc print functions for Ampersand data structures
---------------------------------------
-- LaTeX math markup
---------------------------------------

class ShowMath a where
 showMath :: a -> String
 showMathcode :: Fspc -> a -> String
 showMathcode _ x = showMath x

instance ShowMath Concept where
 showMath c = texOnly_Id(name c)
 showMathcode _ c = texOnly_Id(name c)

instance ShowMath (Gen Concept) where
 showMath g = showMath (genspc g) ++"\\ \\le\\ "++showMath (gengen g)
 showMathcode fSpec g = showMathcode fSpec (genspc g) ++"\\ \\le\\ "++showMathcode fSpec (gengen g)

instance ShowMath (Rule (Relation Concept)) where
 showMath r = error ("!Fatal (module Rendering.PandocAux 456): Please supply specification of the context in showMath "++showADL r)
 showMathcode fSpec r
  = {- ( if isSignal r
      then "\\verb#RULE # "++texId(name r)++"\\ \\verb# SIGNALS #"
      else "\\verb#RULE # "++texId(name r)++"\\ \\verb# MAINTAINS #"
    )++  -}
    case rrsrt r of
      Truth          -> showMathcode fSpec (rrcon r)
      Implication    -> showMathcode fSpec (rrant r) ++"\\ "++texOnly_subs  ++"\\ "++showMathcode fSpec (rrcon r)
      Equivalence    -> showMathcode fSpec (rrant r) ++"\\ "++texOnly_equals++"\\ "++showMathcode fSpec (rrcon r)
      Generalization -> showMathcode fSpec (G (pos r) (source (rrcon r)) (source (rrant r)) "")

instance ShowMath (Expression (Relation Concept)) where
 showMath e           = (showchar.insParentheses) e
 showMathcode fSpec e = (showchar.insParentheses.disambiguate fSpec.relatsoff) e

showchar :: Expression (Relation Concept) -> String
showchar (Tm rel _) = showMath rel
showchar (Fux [])  = "\\cmpl{\\full}"
showchar (Fux fs)  = intercalate "\\cup" [showchar f| f<-fs]     -- union
showchar (Fix [])  = "\\full"
showchar (Fix fs)  = intercalate "\\cap" [showchar f| f<-fs]     -- intersection
showchar (Fdx [])  = "\\cmpl{\\iden}"
showchar (Fdx ts)  = intercalate texOnly_relAdd [showchar t| t<-ts]  -- relative addition (dagger)
showchar (F [])   = "\\iden"
showchar (F ts)   = intercalate texOnly_compose [showchar t| t<-ts] -- relative multiplication (semicolon)
showchar (K0x e')  = "\\kleenestar{"++showchar e'++"}"
showchar (K1x e')  = "\\kleeneplus{"++showchar e'++"}"
showchar (Cpx e')  = "\\cmpl{"++showchar e'++"}"
showchar (Tc f)   = "("++showchar f++")"

instance ShowMath (Relation Concept) where
 showMath rel@(Rel{})
  = if inline rel then mstr else "\\flip{"++mstr++"}"
    where
      mstr  = texOnly_Id(name rel)++
              if null (relats rel)
              then (if     inline rel  && a==source s && b==target s ||
                      not (inline rel) && a==target s && b==source s
                    then "" else showSign [a,b])
              else showSign (relats rel)
      s     = makeDeclaration rel
      a     = source rel
      b     = target rel
 showMath m@(I{})
  = if null (relats m) then "\\iden" else "\\ident{"++showSign (relats m)++"}"
 showMath m@(V{})
  = if null (relats m) then "\\full" else "\\fullt{"++showSign (relats m)++"}"
 showMath m@(Mp1{})
  = "'"++rel1val m++"'"++(showSign [rel1typ m])


instance ShowMath (Declaration Concept) where
 showMath decl@(Sgn{})
  = "\\declare{"++name decl++"}{"++name (source decl)++"}{"++name (target decl)++"}"
 showMath Isn{}
  = "\\iden"
 showMath Vs{}
  = "\\full"
 showMath Iscompl{}
  = "\\cmpl{\\iden}"

latexEscShw :: (Show a) => a -> [Char]
latexEscShw x = show x

-- stripSpecialChars is used inside LaTeX references, where identifiers with underscores cannot be handled.
stripSpecialChars :: [Char] -> [Char] 
stripSpecialChars x 
       = case x of
             []     -> []
             '_':cs -> upCap (stripSpecialChars cs)  -- ^ since underscore is not allowed, use capital instead
             c:cs   -> (if isAscii c 
                       then [c]
                       else "__"++ show (ord c)++"__")  -- TODO: is this allowed in LaTeX references?
                       ++stripSpecialChars cs
             

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
     str = [Emph [Str (unCap c)]
           ,TeX ("\\index{"++unCap c++"}\\marge{"++unCap c++"}") ]


---------------------------
--- LaTeX related stuff ---
---------------------------

texOnly_Id :: String -> String
texOnly_Id s = "\\id{"++escape s++"}"
 where escape "" = ""
       escape ('_': str) = "\\_"++escape str
       escape (c:str)    = c:escape str

texOnly_fun :: String
texOnly_fun = "\\rightarrow"

texOnly_rel :: String
texOnly_rel = "\\times" 

texOnly_relAdd :: String
texOnly_relAdd = "\\dagger"

texOnly_compose :: String
texOnly_compose = ";"

texOnly_subs :: String
texOnly_subs = "\\vdash"

texOnly_equals :: String
texOnly_equals = "=" 
