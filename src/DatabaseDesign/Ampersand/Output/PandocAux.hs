{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Output.PandocAux
      ( writepandoc
      , labeledHeader
    --  , xrefReference
      , symDefLabel, symDefRef
      , symReqLabel, symReqRef, symReqPageRef
    --  , xrefFigure1
      , pandocEqnArray
      , pandocEquation
      , makeDefinition, uniquecds
      , count
      , ShowMath(..)
      , latexEscShw
      , xrefCitation
      , texOnly_Id
      , texOnly_fun
      , texOnly_rel
      , commaEngPandoc, commaNLPandoc
      )
where
import DatabaseDesign.Ampersand.ADL1
import Data.Char hiding (Space)
import Text.Pandoc
import DatabaseDesign.Ampersand.Basics
import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)
import DatabaseDesign.Ampersand.Misc        
import System                 (system, ExitCode(ExitSuccess,ExitFailure))
import System.IO              (hPutStrLn, stderr)
--import System.Process
import System.FilePath        (combine,addExtension,replaceExtension)
import System.Directory
import System.Info (os)
import Data.List              (isInfixOf,intercalate)
import Control.Monad

fatal :: Int -> String -> a
fatal = fatalMsg "Basics"

--DESCR -> functions to write the pandoc
--         String = the name of the outputfile
--         The first IO() is a Pandoc output format
--         The second IO(): If the output format is latex, then this IO() generates a .pdf from the .tex
writepandoc :: Options -> [GlossaryItem] -> Pandoc -> (String,IO(),IO())
writepandoc flags gis thePandoc = (outputFile,makeOutput,postProcessMonad)
         where
         outputFile = addExtension (combine (dirOutput flags) (baseName flags)) 
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
                            verboseLn flags "... done."
              FRtf    -> do verboseLn flags ("Generating to Rich Text Format: "++outputFile)
                            writeFile outputFile (writeRTF ourDefaultWriterOptions{writerTemplate=theTemplate flags gis} thePandoc)
                            verboseLn flags "... done."
              FLatex  -> do verboseLn flags ("Generating to LaTeX: "++outputFile)
                            writeFile outputFile (writeLaTeX ourDefaultWriterOptions{writerTemplate=theTemplate flags gis} thePandoc)
                            verboseLn flags "... done."
              FHtml   -> do verboseLn flags ("Generating to HTML: "++outputFile)
                            writeFile outputFile (writeHtmlString  ourDefaultWriterOptions thePandoc)
                            verboseLn flags "... done."
              FOpenDocument 
                      -> do verboseLn flags ("Generating to Open Document Format: "++outputFile)
                            writeFile outputFile (writeOpenDocument ourDefaultWriterOptions thePandoc)
                            verboseLn flags "... done."
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
                                          if ready then "."
                                                   else ", but did not solve all references!")
                             where 
                                doRestOfPdfLatex :: (Bool,Int) -> IO (Bool,Int)
                                doRestOfPdfLatex (ready, roundsSoFar)
                                  = if ready || roundsSoFar > 4    -- Make sure we will not hit a loop when something is wrong with call to pdfLatex ...
                                    then return (ready, roundsSoFar)
                                    else do callPdfLatexOnce
                                            let needle = "Rerun to get cross-references right." -- This is the text of the LaTeX Warning telling that label(s) may have changed. 
                                            {- The log file should be renamed before reading, because readFile opens the file
                                               for lazy IO. In a next run, pdfLatex will try to write to the log file again. If it
                                               was read using readFile, it will fail because the file is still open. 8-((  
                                            -} 
                                            renameFile (replaceExtension outputFile "log") (replaceExtension outputFile ("log"++show roundsSoFar))
                                            haystack <- readFile (replaceExtension outputFile ("log"++show roundsSoFar))  
                                            let notReady =  needle `isInfixOf` haystack
                                            when notReady (verboseLn flags "Another round of pdfLatex is required. Hang on...")
                                          --  when notReady (dump "log")  -- Need to dump the last log file, otherwise pdfLatex cannot write its log.
                                            doRestOfPdfLatex (not notReady, roundsSoFar +1)

                                callPdfLatexOnce :: IO ()
                                callPdfLatexOnce = 
                                   do result <- if os `elem` ["mingw32","mingw64","cygwin","windows"] --REMARK: not a clear enum to check for windows OS
                                                then system ( pdfLatexCommand++
                                                              if verboseP flags then "" else "> "++combine (dirOutput flags) "pdflog" ) >>
                                                     system  makeIndexCommand
                                                --REMARK: MikTex is windows; Tex-live does not have the flag -include-directory.
                                                else system ( "cd "++dirOutput flags++
                                                              " && pdflatex "++commonFlags++
                                                              texFilename ++ if verboseP flags then "" else "> pdflog" ) >>
                                                     system makeIndexCommand
                                      case result of 
                                         ExitSuccess   -> verboseLn flags "PDF file created."
                                         ExitFailure _ -> hPutStrLn stderr $  if verboseP flags 
                                                                              then "" -- in verbose mode, Latex already gave plenty of information
                                                                              else "\nLatex error.\nFor more information, run pdflatex on "++texFilename++
                                                                                    " or rerun ampersand with the --verbose option"
                                      where
                                      pdfLatexCommand = "pdflatex "++commonFlags++pdfflags++ outputFile
                                      --makeIndexCommand = "makeglossaries "++replaceExtension outputFile "glo"
                                      --makeindex uses the error stream for verbose stuff...
                                      makeIndexCommand = "makeindex -s "++replaceExtension outputFile "ist"++" -t "++replaceExtension outputFile "glg"++" -o "++replaceExtension outputFile "gls"++" "++replaceExtension outputFile "glo 2> "++combine (dirOutput flags) "glossaries.log"
                                      pdfflags = (if verboseP flags then "" else " --disable-installer") ++
                                                 " -include-directory="++dirOutput flags++ " -output-directory="++dirOutput flags++" "
                                      texFilename = addExtension (baseName flags) ".tex"
                                      commonFlags = if verboseP flags then "" else "--interaction=nonstopmode " -- MacTex options are normally with one '-', but '--interaction' is accepted 
                                      -- when verbose is off, let latex halt on error to prevent waiting for user input without prompting for it
                                      -- on windows, we also do --disable-installer, since otherwise a missing package may cause interaction,
                                      -- even with --interaction=nonstopmode.
               _  -> return()            

-- | The definitions of concepts will be written in the glossary
type GlossaryItem = A_Concept --change to a data type if you want more types of glossary items
  
-- TODO: Han, wil jij nog eens goed naar de PanDoc template kijken.
-- De onderstaande code is een vrij rauwe combinatie van de oude LaTeX header en het
-- default PanDoc template. Dat krijg je door op de command line   pandoc -D latex  uit te voeren.
-- In elk geval moeten de conditionals in LaTeX eruit en vervangen worden door Haskell conditionals.
-- Wellicht wordt e.e.a. daardoor simpeler.
theTemplate :: Options -> [GlossaryItem] -> String
theTemplate flags gis 
  = case fspecFormat flags of
    FLatex ->  concat $
               [ "% This header is the default LaTeX template for generating documents with Ampersand.\n"
               , "% It was generated with "++ampersandVersionStr++"\n"
               , "% You can modify this file to make it fit your needs. However, the required knowledge of \n"
               , "% LaTeX is not documented here. You can find help with that at http://en.wikibooks.org/wiki/LaTeX\n"
               , "% see the ampersand user guide (TODO) for more information on how to apply your own LaTeX header\n"
               , "%\n"
               , "\\documentclass[10pt,a4paper]{report}              % Define the document class\n"
               , "\\parskip 12pt plus 2.5pt minus 4pt                % Extra vertical space between paragraphs.\n"
               , "\\parindent 0em                                    % Width of paragraph indentation.\n\n"
               , "% -- packages used for several purposes:\n"
               , "\\usepackage{float}\n"
               , "\\usepackage{ctable}\n"
               , "\\usepackage{theorem}\n"
               , "\\usepackage{amssymb}\n"
               , "\\usepackage{amsmath}         % Provides various features to facilitate writing math formulas and to improve the typographical quality of their output.\n"
               , "\\usepackage{breqn}\n"
               , "\\usepackage{colonequals}\n"
            --   , "\\usepackage{hyperref}\n"
               , "\\usepackage{ucs}             % Provides various features for UTF8 (internationalization) stuff\n"
               , "\\usepackage[utf8x]{inputenc} %\n"
               ] ++
               ( case theme flags of
                  ProofTheme -> [ "\\usepackage[landscape]{geometry}\n"
                                , "%http://www.phil.cam.ac.uk/teaching_staff/Smith/logicmatters/l4llogiciansnew.html\n"
                                , "%http://www.phil.cam.ac.uk/teaching_staff/Smith/LaTeX/guides/BussGuide2.pdf\n"
                                , "\\usepackage{bussproofs}\n"
                                , "\\def\\defaultHypSeparation{\\hskip.0in}\n"
                                , "\\def\\ScoreOverhang{1pt}\n"]
                  _ -> []
               )++
               ( case language flags of
                  Dutch   -> [ "\\usepackage[dutch]{babel}\n"
                             , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{definition}{Definitie}[section]\n"
                             , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{designrule}[definition]{Functionele eis}\n" ]
                  English -> [ "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{definition}{Definition}[section]\n"
                             , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{designrule}[definition]{Requirement}\n" ]
               )++
               [ "\\usepackage{graphicx}\n" | genGraphics flags] ++
          --     ["\\graphicspath{{"++posixFilePath (dirOutput flags)++"}}" {- | graphics flags, equalFilePath (dirOutput flags) "." -}] ++  -- for multiple directories use \graphicspath{{images_folder/}{other_folder/}{third_folder/}}
               [ "\\def\\id#1{\\mbox{\\em #1\\/}}\n"
               , "\\newcommand{\\marge}[1]{\\marginpar{\\begin{minipage}[t]{3cm}{\\noindent\\small\\em #1}\\end{minipage}}}\n"
               , "\\def\\define#1{\\label{dfn:#1}\\index{#1}{\\em #1}}\n"
               , "\\def\\defmar#1{\\label{dfn:#1}\\index{#1}\\marge{#1}{\\em #1}}\n"
               , "\\newcommand{\\iden}{\\mathbb{I}}\n"
               , "\\newcommand{\\ident}[1]{\\mathbb{I}_{#1}}\n"
               , "\\newcommand{\\full}{\\mathbb{V}}\n"
               , "\\newcommand{\\fullt}[1]{\\mathbb{V}_{[#1]}}\n"
           --    , "\\newcommand{\\relAdd}{\\dagger}\n"
               , "\\newcommand{\\flip}[1]{{#1}^\\smallsmile} %formerly:  {#1}^\\backsim\n"
               , "\\newcommand{\\kleeneplus}[1]{{#1}^{+}}\n"
               , "\\newcommand{\\kleenestar}[1]{{#1}^{*}}\n"
               , "\\newcommand{\\asterisk}{*}\n"
               , "\\newcommand{\\cmpl}[1]{\\overline{#1}}\n"
           --    , "\\newcommand{\\compose}{;}\n"
               , "\\newcommand{\\subs}{\\vdash}\n"
               , "\\newcommand{\\rel}{\\times}\n"
               , "\\newcommand{\\fun}{\\rightarrow}\n"
               , "\\newcommand{\\isa}{\\sqsubseteq}\n"
               , "\\newcommand{\\N}{\\mbox{\\msb N}}\n"
               , "\\newcommand{\\disjn}[1]{\\id{disjoint}(#1)}\n"
               , "\\newcommand{\\fsignat}[3]{\\id{#1}:\\id{#2}\\fun\\id{#3}}\n"
               , "\\newcommand{\\signat}[3]{\\id{#1}:\\id{#2}\\rel\\id{#3}}\n"
               , "\\newcommand{\\signt}[2]{\\mbox{\\({#1}_{[{#2}]}\\)}}\n"
               , "\\newcommand{\\declare}[3]{\\id{#1}:\\ \\id{#2}\\rel\\id{#3}}\n"
               , "\\newcommand{\\fdeclare}[3]{\\id{#1}:\\ \\id{#2}\\fun\\id{#3}}\n"
               ] ++ ["\\selectlanguage{dutch}\n" | language flags == Dutch ] ++
               [ "% =====================================================================================\n"
               , "% == The hyperref package will take care of turning all internal references of your  ==\n"
               , "% == document into hyperlinks. For this to work properly some magic is necessary,    ==\n"
               , "% == so you have to put \\usepackage[pdftex]{hyperref} as the last command            ==\n"
               , "% == into the preamble of your document.                                             ==\n"
               , "% == Many options are available to customize the behaviour of the hyperref           ==\n"
               , "% == package. See http://ctan.tug.org/tex-archive/info/lshort/english/lshort.pdf     ==\n"
               , "% =====================================================================================\n"
               , "\\usepackage[pdftex,colorlinks=false]{hyperref}\n"
               ] ++
               [ "%  -- end of Ampersand specific header. The remainder is PanDoc-specific. run C:>pandoc -D latex  to see the default template.\n"
          {-TODO: disabled while running on icommas.ou.nl (uses MikTex 2.5 i.e. without xetex)
               , "$if(xetex)$\n"
               , "\\usepackage{ifxetex}\n"
               , "\\ifxetex\n"
               , "  \\usepackage{fontspec,xltxtra,xunicode}\n"
               , "  \\defaultfontfeatures{Mapping=tex-text,Scale=MatchLowercase}\n"
               , "\\else\n"
               , "  \\usepackage[mathletters]{ucs}\n"
               , "  \\usepackage[utf8x]{inputenc}\n"
               , "\\fi\n"
               , "$else$\n"
               , "\\usepackage[mathletters]{ucs}\n"
               , "\\usepackage[utf8x]{inputenc}\n"
               , "$endif$\n" -}
               , "$if(lhs)$\n"
               , "\\usepackage{listings}\n"
               , "\\lstnewenvironment{code}{\\lstset{language=Haskell,basicstyle=\\small\\ttfamily}}{}\n"
               , "$endif$\n"
               , "\\setlength{\\parindent}{0pt}\n"
               , "\\setlength{\\parskip}{6pt plus 2pt minus 1pt}\n"
               , "$if(verbatim-in-note)$\n"
               , "\\usepackage{fancyvrb}\n"
               , "$endif$\n"
               , "$if(fancy-enums)$\n"
               , "\\usepackage{enumerate}\n"
               , "$endif$\n"
               , "$if(tables)$\n"
               , "\\usepackage{array}\n"
               , "% This is needed because raggedright in table elements redefines \\\\:\n"
               , "\\newcommand{\\PreserveBackslash}[1]{\\let\\temp=\\\\#1\\let\\\\=\\temp}\n"
               , "\\let\\PBS=\\PreserveBackslash\n"
               , "$endif$\n"
               , "$if(strikeout)$\n"
               , "\\usepackage[normalem]{ulem}\n"
               , "$endif$\n"
               , "$if(subscript)$\n"
               , "\\newcommand{\\textsubscr}[1]{\\ensuremath{_{\\scriptsize\\textrm{#1}}}}\n"
               , "$endif$\n"
               , "$if(links)$\n"
               , "\\usepackage[breaklinks=true]{hyperref}\n"
               , "$endif$\n"
               , "$if(url)$\n"
               , "\\usepackage{url}\n"
               , "$endif$\n"
               , "$if(numbersections)$\n"
               , "$else$\n"
               , "\\setcounter{secnumdepth}{0}\n"
               , "$endif$\n"
               , "$if(verbatim-in-note)$\n"
               , "\\VerbatimFootnotes % allows verbatim text in footnotes\n"
               , "$endif$\n"
               , "$for(header-includes)$\n"
               , "$header-includes$\n"
               , "$endfor$\n"
               , "\n"
               , "$if(title)$\n"
               , "\\title{$title$}\n"
               , "$endif$\n"
               , "\\author{$for(author)$$author$$sep$\\\\$endfor$}\n"
               , "$if(date)$\n"
               , "\\date{$date$}\n"
               , "$endif$\n"
               , "\n"
                 {- "Note that the glossaries package must be loaded after the hyperref package (contrary
                  -  to the general advice that hyperref should be loaded last). The glossaries
                  -  package should also be loaded after html, inputenc, babel and ngerman."
                  - http://ftp.snt.utwente.nl/pub/software/tex/macros/latex/contrib/glossaries/glossariesbegin.pdf -}
               , "\\usepackage{glossaries}\n"
               , "\\makeglossaries\n"
               ] ++
               [ "\\newglossaryentry{"++latexEscShw cdnm ++"}{name={"++latexEscShw (name c)++"}, description={"++latexEscShw (cddef cd)++"}}\n" 
               | c<-gis, (cdnm,cd)<-uniquecds c]
               ++
               [ "\\begin{document}\n"
               , "$if(title)$\n"
               , "\\maketitle\n"
               , "$endif$\n"
               ] ++
               ( if diagnosisOnly flags
                 then []
                 else [ "\n"
                      , "$if(toc)$\n"
                      , "\\tableofcontents\n"
                      , "\n"
                      , "$endif$\n"
                      ] ) ++
               [ "$body$\n"
               , "\\end{document}\n"
               ]
    FRtf ->    concat
               [ "$if(legacy-header)$\n"
               , "$legacy-header$\n"
               , "$else$\n"
               , "{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 \\fswiss Helvetica;}{\\f1 Courier;}}\n"
               , "{\\colortbl;\\red255\\green0\\blue0;\\red0\\green0\\blue255;}\n"
               , "\\widowctrl\\hyphauto\n"
               , "$endif$\n"
               , "$for(header-includes)$\n"
               , "$header-includes$\n"
               , "$endfor$\n"
               , "\n"
               , "$if(title)$\n"
               , "{\\pard \\qc \\f0 \\sa180 \\li0 \\fi0 \\b \\fs36 $title$\\par}\n"
               , "$endif$\n"
               , "$for(author)$\n"
               , "$endfor$\n"
               , "$if(date)$\n"
               , "{\\pard \\qc \\f0 \\sa180 \\li0 \\fi0  $date$\\par}\n"
               , "$endif$\n"
               , "$if(spacer)$\n"
               , "{\\pard \\ql \\f0 \\sa180 \\li0 \\fi0 \\par}\n"
               , "$endif$\n"
               , "$for(include-before)$\n"
               , "$include-before$\n"
               , "$endfor$\n"
               , "$body$\n"
               , "$for(include-after)$\n"
               , "$include-after$\n"
               , "$endfor$\n"
               , "latex}\n"
               ]
    FPandoc       -> fatal 320 "No template defined for Pandoc output"
    FOpenDocument -> fatal 321 "No template defined for ODF output"
    FHtml         -> fatal 322 "No template defined for HTML output"
 
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
  symReqLabel  :: a -> String  -- labels the requirement of a
  symReqLabel   c = "\\label{Req"++symLabel c++"}"
  symDefLabel  :: a -> String  -- labels the definition of a
  symDefLabel   c = "\\label{Def"++symLabel c++"}"
  symReqRef    :: a -> String  -- references the requirement of a
  symReqRef     c = "\\ref{Req"++symLabel c++"}"
  symDefRef    :: a -> String  -- references the definition of a 
  symDefRef     c = "\\ref{Def"++symLabel c++"}"
  symReqPageRef :: a -> String  -- references the requirement of a
  symReqPageRef c = "\\pageref{Req"++symLabel c++"}"
  symDefPageRef :: a -> String  -- references the definition of a 
  symDefPageRef c = "\\pageref{Def"++symLabel c++"}"

instance SymRef ConceptDef where
  symLabel cd = "Concept:"++stripSpecialChars (cdcpt cd)

instance SymRef A_Concept where
  symLabel c = "Concept:"++stripSpecialChars (name c)

instance SymRef Declaration where
  symLabel d = "Decl:"++stripSpecialChars (name d++name (source d)++name (target d))

instance SymRef Rule where
  symLabel r = "Rule:"++stripSpecialChars (name r)

--   xrefChptReference :: String -> [Inline]
--   xrefChptReference myLabel = [RawInline "latex" ("\\ref{section:"++myLabel++"}")] --TODO werkt nog niet correct
---   xrefTableReference :: String -> [Inline]
--   xrefTableReference myLabel = [RawInline "latex" ("\\ref{tab:"++myLabel++"}")]
-- BECAUSE: Why (SJ) does the code of labeledHeader look stupid?
--         When Han looked at pandoc code TexInfo.hs blockToTexinfo ln.208,
--         he expected chapter,section,sub,subsub respectively.
--         However, he got section,sub,subsub,plain text respectively.
--         Looks like an error in PanDoc, doesn't it?
--         So now he wrote chapters as 0 setting a [Inline] -> [RawInline "latex" "\\chapter{...}"].
--         We do not know yet how this relates to other formats like rtf.

labeledHeader :: Int -> String -> String -> [Block]
labeledHeader 0 lbl str =
                 [Para [RawInline "latex" ("\\chapter{"++latexEscShw str++"}"), xrefLabel lbl]]
labeledHeader lev lbl str =
                 Header lev [Str str]
                 : [Para [xrefLabel lbl]]
 
xrefLabel :: String -> Inline        -- uitbreidbaar voor andere rendering dan LaTeX
xrefLabel myLabel = RawInline "latex" ("\\label{"++myLabel++"}")
xrefCitation :: String -> Inline    -- uitbreidbaar voor andere rendering dan LaTeX
xrefCitation myLabel = RawInline "latex" ("\\cite{"++myLabel++"}")


-- Para [Math DisplayMath "\\id{aap}=A\\times B\\\\\n\\id{noot}=A\\times B\n"]
pandocEqnArray :: [(String,String,String)] -> [Block]
pandocEqnArray xs
 = [ Para [ RawInline "latex" ("\\begin{eqnarray}\n   "++
                               intercalate "\\\\\n   " [ a++"&"++b++"&"++c | (a,b,c)<-xs ]++
                               "\n\\end{eqnarray}") ]
   | not (null xs)]
   
pandocEquation :: String -> [Block]
pandocEquation x
 = [ Para [ RawInline "latex" ("\\begin{dmath}[compact]\n   "++ x ++"\n\\end{dmath}") ]
   | not (null x)]

--DESCR -> pandoc print functions for Ampersand data structures
---------------------------------------
-- LaTeX math markup
---------------------------------------

class ShowMath a where
 showMath :: a -> String

instance ShowMath A_Concept where
 showMath c = texOnly_Id (name c)

instance ShowMath A_Gen where
 showMath g     = showMath (genspc g)           ++"\\ \\le\\ "++showMath           (gengen g)

instance ShowMath Rule where
 showMath r = showMath (rrexp r)

instance ShowMath Expression where
 showMath             = showchar.insParentheses

showchar :: Expression -> String
showchar (EEqu (r,s)) = showchar r ++ texOnly_equals ++ showchar s
showchar (EImp (r,s)) = showchar r ++ texOnly_subs ++ showchar s
showchar (EIsc [])    = "V"
showchar (EIsc fs)    = intercalate texOnly_inter [showchar f | f<-fs]     -- intersection
showchar (EUni [])    = "-V"
showchar (EUni fs)    = intercalate texOnly_union [showchar f | f<-fs]     -- union
showchar (EDif (r,s)) = concat[showchar r | not(isTrue r)] ++ texOnly_bx ++ showchar s
showchar (ELrs (r,s)) = showchar r ++ texOnly_lRes ++ showchar s
showchar (ERrs (r,s)) = showchar r ++ texOnly_rRes ++ showchar s
showchar (ECps [])    = "I"
showchar (ECps ts)    = intercalate texOnly_compose [showchar t | t<-ts] -- relative multiplication (semicolon)
showchar (ERad [])    = "-I[?]"
showchar (ERad ts)    = intercalate texOnly_relAdd [showchar t | t<-ts]  -- relative addition (dagger)
showchar (EPrd [])    = "ONE"
showchar (EPrd ts)    = intercalate texOnly_crtPrd [showchar t | t<-ts]  -- cartesian product (asterisk)
showchar (EKl0 e')    = showchar e'++"^{"++texOnly_star++"}"
showchar (EKl1 e')    = showchar e'++"^{"++texOnly_plus++"}"
showchar (ECpl e')    = "\\cmpl{"++showchar e'++"}"
showchar (EFlp e')    = showchar e'++"^{"++texOnly_flip++"}"
showchar (ETyp e' sgn)
       | isEndo sgn = showchar e' ++ "_{["++name (source sgn)++"]}"
       | otherwise  = showchar e' ++ "_{["++name (source sgn)++texOnly_rel++name (target sgn)++"]}"
showchar (EBrk f)     = "("++showchar f++")"
showchar (ERel r)     = showMath r

instance ShowMath Relation where
 showMath rel@(Rel{})
  = (texOnly_Id.name) rel
 showMath I{}
  = "I"
 showMath V{}
  = "V"
 showMath r@(Mp1{})
  = "'"++relval r++"'"

instance ShowMath Declaration where
 showMath decl@(Sgn{})
  = "\\declare{"++latexEscShw(name decl)++"}{"++latexEscShw(name (source decl))++"}{"++latexEscShw(name (target decl))++"}"
 showMath Isn{}
  = "\\iden"
 showMath Vs{}
  = "\\full"
 showMath Iscompl{}
  = "\\cmpl{\\iden}"

-- | latexEscShw escapes to LaTeX encoding. It is intended to be used in LaTeX text mode.
--   For more elaborate info on LaTeX encoding, consult the The Comprehensive LATEX Symbol List
--   on:    http://ftp.snt.utwente.nl/pub/software/tex/info/symbols/comprehensive/symbols-a4.pdf
latexEscShw :: String -> String
latexEscShw "" = ""
latexEscShw ('$': str)  = "\\$"++latexEscShw str
latexEscShw ('^': str)  = "\\^{}"++latexEscShw str
latexEscShw ('~': str)  = "\\~{}"++latexEscShw str
latexEscShw ('%': str)  = "\\%"++latexEscShw str
latexEscShw ('_': str)  = "\\_"++latexEscShw str
latexEscShw ('{': str)  = "\\{"++latexEscShw str
latexEscShw ('}': str)  = "\\}"++latexEscShw str
latexEscShw ('&': str)  = "\\&"++latexEscShw str
latexEscShw ('#': str)  = "\\#"++latexEscShw str
latexEscShw ('\\': str) = "\\textbackslash "++latexEscShw str
latexEscShw ('|': str)  = "\\textbar "      ++latexEscShw str
latexEscShw ('>': str)  = "\\textgreater "  ++latexEscShw str
latexEscShw ('<': str)  = "\\textless "     ++latexEscShw str
latexEscShw ('\"': str) = "\\textquotedbl " ++latexEscShw str
latexEscShw (c:str)     = c:latexEscShw str

-- stripSpecialChars is used inside LaTeX references, where identifiers with underscores cannot be handled.
stripSpecialChars :: String -> String
stripSpecialChars x 
       = case x of
             []     -> []
             '_':cs -> upCap (stripSpecialChars cs)  -- since underscore is not allowed, use capital instead
             c:cs   -> (if isAscii c 
                       then [c]
                       else "__"++ show (ord c)++"__")  -- TODO: is this allowed in LaTeX references?
                       ++stripSpecialChars cs
             

--posixFilePath :: FilePath -> String
-- tex uses posix file notation, however when on a windows machine, we have windows conventions for file paths...
-- To set the graphicspath, we want something like: \graphicspath{{"c:/data/ADL/output/"}}
--posixFilePath fp = "/"++System.FilePath.Posix.addTrailingPathSeparator (System.FilePath.Posix.joinPath   (tail  (splitDirectories fp)))

uniquecds :: A_Concept -> [(String,ConceptDef)]
uniquecds c = [(if length(cptdf c)==1 then cdcpt cd else cdcpt cd++show i , cd) | (i,cd)<-zip [(1::Integer)..] (cptdf c)]
makeDefinition :: Options -> (Int, String,ConceptDef) -> [Block]
makeDefinition flags (i,cdnm,cd)
 = case fspecFormat flags of
    FLatex ->  [ Para ( (if i==0 then [ RawInline "latex" (symDefLabel cd++"\n") ] else [])++
                        [ RawInline "latex" ("\\gls{"++latexEscShw cdnm++"}\n") ] ++
                        [Str (latexEscShw (cddef cd))] ++ [ Str (" ["++latexEscShw (cdref cd)++"]") | not (null (cdref cd)) ]
                      )
               ]
    _      ->  [ Para ( [Str (cddef cd)] ++ [ Str (" ["++cdref cd++"]") | not (null (cdref cd)) ] )
               ]
{- used to be the following code, but that is maybe too patronizing. For the time being, we'll let makeDefinition stay. It might be removed later, after the cdef appears to be useful.
  = case language flags of
     English -> [Str ("A"++(if unCap (take 1 c) `elem` ["a","e","i","o","u"] then "n" else "")++" ")]++str++[Str (" is "++cdef)]
     Dutch   -> [Str "Een "]++str++[Str (" is "++cdef)]
    where
     str = [Emph [Str (latexEscShw (unCap c))]
           ,RawInline "latex" ("\\index{"++latexEscShw (unCap c)++"}\\marge{"++latexEscShw (unCap c)++"}") ]
-}

commaEngPandoc :: Inline -> [Inline] -> [Inline]
commaEngPandoc str [a,b,c]= [a,Str ", ",b,Str ", ",str, Str " ", c]
commaEngPandoc str [a,b]  = [a,Str " ",str, Str " ", b]
commaEngPandoc _   [a]    = [a]
commaEngPandoc str (a:as) = [a, Str ", "]++commaEngPandoc str as
commaEngPandoc _   []     = []

commaNLPandoc :: Inline -> [Inline] -> [Inline]
commaNLPandoc str [a,b]  = [a,Str " ",str, Str " ", b]
commaNLPandoc  _  [a]    = [a]
commaNLPandoc str (a:as) = [a, Str ", "]++commaNLPandoc str as
commaNLPandoc  _  []     = []

---------------------------
--- LaTeX related stuff ---
---------------------------

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

texOnly_flip :: String
texOnly_flip = "\\smallsmile "
