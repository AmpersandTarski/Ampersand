{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Output.PandocAux
      ( writepandoc
      , labeledThing
    --  , xrefReference
      , symDefLabel, symDefRef
      , symReqLabel, symReqRef, symReqPageRef
      , xrefSupported
    --  , xrefFigure1
      , pandocEqnArray
      , pandocEquation
      , makeDefinition, uniquecds
      , count
      , ShowMath(..)
      , latexEscShw, escapeNonAlphaNum
      , xrefCitation
      , texOnly_Id
      , texOnly_fun
      , texOnly_rel
      , commaEngPandoc, commaNLPandoc
      )
where
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Fspec
import Data.Char hiding (Space)
import Text.Pandoc
import Text.Pandoc.Builder
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Basics hiding (hPutStrLn)
import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)
import DatabaseDesign.Ampersand.Misc        
import System.Process      (system)
import System.Exit         (ExitCode(ExitSuccess,ExitFailure))
import System.IO              (hPutStrLn, stderr)
import Paths_ampersand
import System.FilePath       -- (combine,addExtension,replaceExtension)
import System.Directory
import System.Info (os)
import Data.List              (isInfixOf,intercalate)
import Control.Monad
import Data.Maybe

fatal :: Int -> String -> a
fatal = fatalMsg "Output.PandocAux"


-- | Default key-value pairs for use with the Pandoc template
defaultWriterVariables :: Options -> Fspc -> [(String , String)]
defaultWriterVariables flags fSpec
  = [ ("title", (case (language flags, diagnosisOnly flags) of
                        (Dutch  , False) -> "Functionele Specificatie van "
                        (English, False) -> "Functional Specification of "
                        (Dutch  ,  True) -> "Diagnose van "
                        (English,  True) -> "Diagnosis of " 
                )++name fSpec)
 --   , ("mainfont",
 --   , ("sansfont",
 --   , ("monofont",
 --   , ("mathfont",
    , ("fontsize", "10pt,a4paper")   --can be overridden by geometry package (see below)
    , ("lang"    , case language flags of
                       Dutch   -> "dutch"
                       English -> "english")
    , ("mainlang", case language flags of
                       Dutch   -> "dutch"
                       English -> "english")
    , ("documentclass","report")
    ] ++
    [ ("toc" , "<<TheTableOfContentsShouldGoHere>>") | not (diagnosisOnly flags)]++
    [ ("header-includes", unlines 
         [ "% ============Ampersand specific Begin================="
         , "\\usepackage[toc]{glossaries}    % package used to define terms"
         , "\\usepackage{breqn}"
         , "\\usepackage{colonequals}"
         , "% == [all]{hypcap} after {hyperref} shows the ref'd picture i.o. the caption @ click =="
         , "\\usepackage[all]{hypcap}"
         , "\\def\\id#1{\\mbox{\\em #1\\/}}"
         , "\\newcommand{\\marge}[1]{\\marginpar{\\begin{minipage}[t]{3cm}{\\noindent\\small\\em #1}\\end{minipage}}}"
         , "\\def\\define#1{\\label{dfn:#1}\\index{#1}{\\em #1}}"
         , "\\def\\defmar#1{\\label{dfn:#1}\\index{#1}\\marge{#1}{\\em #1}}"
         , "%\\newcommand{\\iden}{\\mathbb{I}}"
         , "%\\newcommand{\\ident}[1]{\\mathbb{I}_{#1}}"
         , "\\newcommand{\\full}{\\mathbb{V}}"
         , "\\newcommand{\\fullt}[1]{\\mathbb{V}_{[#1]}}"
         , "\\newcommand{\\flip}[1]{{#1}^\\smallsmile} %formerly:  {#1}^\\backsim"
         , "\\newcommand{\\kleeneplus}[1]{{#1}^{+}}"
         , "\\newcommand{\\kleenestar}[1]{{#1}^{*}}"
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
         , "\\newcommand{\\fdeclare}[3]{\\id{#1}:\\ \\id{#2}\\fun\\id{#3}}"
         , "% ============Ampersand specific End==================="
         ])
--    , ("geometry", "margin=2cm, a4paper")
    ]
         
     
--DESCR -> functions to write the pandoc
--         String = the name of the outputfile
--         The first IO() is a Pandoc output format
--         The second IO(): If the output format is latex, then this IO() generates a .pdf from the .tex
writepandoc :: Options -> Fspc -> Pandoc -> (String,IO(),IO())
writepandoc flags fSpec thePandoc = (outputFile,makeOutput,postProcessMonad)
         where
         outputFile = addExtension (combine (dirOutput flags) (baseName flags)) 
                                       (case fspecFormat flags of        
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
                                                 FLatex        -> ".tex"
                                                 Fhtml         -> ".html"
                                                 Fopendocument -> ".odt"
                                                 Ftexinfo      -> ".texinfo"
                                                 Ftextile      -> ".textile"
                                       )
         makeOutput
            =  do template <- readDefaultTemplate fSpecFormatString
                  verboseLn flags ("Generating "++fSpecFormatString++" to : "++outputFile)
                  writeFile outputFile (pandocWriter (writerOptions template) thePandoc)
                  verboseLn flags "Variables to set in the template:"
                  verboseLn flags (intercalate "\n   " (map show (writerVariables (writerOptions template))))
                  verboseLn flags "... done."
           where 
              pandocWriter :: WriterOptions -> Pandoc -> String
              pandocWriter =
                case fspecFormat flags of
                  Fasciidoc -> fatal 99 "No current support for asciidoc" 
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
                case fspecFormat flags of
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
              readDefaultTemplate :: String -> IO(Maybe String)
              readDefaultTemplate  s = 
                do { dataDir <- getDataDir
                   ; let fp = dataDir </> "outputTemplates" </> "default."++s
                   ; exists <- doesFileExist fp
                   ; (if exists 
                      then do contents <- readFile fp
                              return $ Just contents 
                      else do verboseLn flags $ "Template file does not exist: "++fp
                              verboseLn flags "...trying without template...(but you might want to reinstall ampersand...)" 
                              return Nothing 
                     )
                   } 
              writerOptions :: Maybe String -> WriterOptions
              writerOptions template = case theme flags of
                          ProofTheme -> ampersandDefaultWriterOptions 
                                           { writerTableOfContents=False
                                           , writerNumberSections=False
                                           }
                          _          -> ampersandDefaultWriterOptions
                     where
                       ampersandDefaultWriterOptions =
                         def
                            { writerStandalone=isJust template
                            , writerTableOfContents=True
                            , writerNumberSections=True
                            , writerTemplate=fromMaybe "" template
                            , writerVariables=defaultWriterVariables flags fSpec}
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
                                                              texFilename ++ if verboseP flags then "" else "> "++addExtension(baseName flags) ".pdflog" ) >>
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

-----Linguistic goodies--------------------------------------

count :: Options -> Int -> String -> String
count flags n x
 = case (language flags, n) of
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

class SymRef a where
  symLabel :: a -> String -- unique label for symbolic reference purposes
  symReqLabel :: a -> String  -- labels the requirement of a
  symReqLabel   c = "\\label{Req"++symLabel c++"}"
  symDefLabel :: a -> String  -- labels the definition of a
  symDefLabel   c = "\\label{Def"++symLabel c++"}"
  symReqRef :: a -> String  -- references the requirement of a
  symReqRef     c = "\\ref{Req"++symLabel c++"}"
  symDefRef :: a -> String  -- references the definition of a 
  symDefRef     c = "\\ref{Def"++symLabel c++"}"
  symReqPageRef :: a -> String  -- references the requirement of a
  symReqPageRef c = "\\pageref{Req"++symLabel c++"}"
  symDefPageRef :: a -> String  -- references the definition of a 
  symDefPageRef c = "\\pageref{Def"++symLabel c++"}"

instance SymRef ConceptDef where
  symLabel cd = "Concept:"++escapeNonAlphaNum (cdcpt cd)

instance SymRef A_Concept where
  symLabel c = "Concept:"++escapeNonAlphaNum (name c)

instance SymRef Declaration where
  symLabel d = "Decl:"++escapeNonAlphaNum (name d++name (source d)++name (target d))

instance SymRef Rule where
  symLabel r = "Rule:"++escapeNonAlphaNum (name r)

instance SymRef PlugInfo where
  symLabel p = "PlugInfo "++escapeNonAlphaNum (name p)

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

-- TODO: Fix the other labeled 'things', to make a neat reference.
labeledThing :: Options -> Int -> String -> String -> Blocks
labeledThing flags lev lbl t =
    header (lev+1) 
       ((text t <> xrefLabel flags lbl))  
 
-- | A label that can be cross referenced to. (only for output formats that support this feature)
xrefLabel :: Options -> String -> Inlines        -- uitbreidbaar voor andere rendering dan LaTeX
xrefLabel flags myLabel
   = if xrefSupported flags
     then rawInline "latex" ("\\label{"++escapeNonAlphaNum myLabel++"}")
     else fatal 508 "Illegal use of xrefLabel."
xrefSupported :: Options -> Bool
xrefSupported flags = fspecFormat flags `elem` [FLatex] 
xrefCitation :: String -> Inline    -- uitbreidbaar voor andere rendering dan LaTeX
xrefCitation myLabel = RawInline "latex" ("\\cite{"++escapeNonAlphaNum myLabel++"}")


-- Para [Math DisplayMath "\\id{aap}=A\\times B\\\\\n\\id{noot}=A\\times B\n"]
pandocEqnArray :: [(String,String,String)] -> [Block]
pandocEqnArray xs
 = [ Para [ RawInline "latex" ("\\begin{eqnarray}\n   "++
                               intercalate "\\\\\n   " [ a++"&"++b++"&"++c | (a,b,c)<-xs ]++
                               "\n\\end{eqnarray}") ]
   | not (null xs)]
   
pandocEquation :: String -> [Block]
pandocEquation x
 = [ Para [ RawInline "latex" ("\\begin{dmath}\n   "++ x ++"\n\\end{dmath}") ]
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

instance ShowMath Sign where
 showMath (Sign s t) = showMath s++"\\rel"++showMath t

instance ShowMath Expression where
 showMath = showExpr . insParentheses
   where  showExpr (EEqu (l,r) _) = showExpr l++texOnly_equals++showExpr r
          showExpr (EImp (l,r) _) = showExpr l++texOnly_subs++showExpr r
          showExpr (EIsc (l,r) _) = showExpr l++texOnly_inter++showExpr r
          showExpr (EUni (l,r) _) = showExpr l++texOnly_union++showExpr r
          showExpr (EDif (l,r) _) = showExpr l++texOnly_bx ++showExpr r
          showExpr (ELrs (l,r) _) = showExpr l++texOnly_lRes++showExpr r
          showExpr (ERrs (l,r) _) = showExpr l++texOnly_rRes++showExpr r
          showExpr (ECps (l,r) _) = showExpr l++texOnly_compose++showExpr r
          showExpr (ERad (l,r) _) = showExpr l++texOnly_relAdd++showExpr r
          showExpr (EPrd (l,r) _) = showExpr l++texOnly_crtPrd++showExpr r
          showExpr (EKl0 e _)     = showExpr (addParensToSuper e)++"^{"++texOnly_star++"}"
          showExpr (EKl1 e _)     = showExpr (addParensToSuper e)++"^{"++texOnly_plus++"}"
          showExpr (EFlp e _)     = showExpr (addParensToSuper e)++"^{"++texOnly_flip++"}"
          showExpr (ECpl e _)     = "\\cmpl{"++showExpr e++"}"
          showExpr (EBrk e)       = "("++showExpr e++")"
          showExpr (ETyp e sgn) 
           | source sgn==target sgn = showExpr e++"_{["++show (source sgn)++"]}"
           | otherwise              = showExpr e++"_{["++show (source sgn)++texOnly_rel++show (target sgn)++"]}"
          -- relations in expressions are printed without type signature, use ETyp to print signatures
          showExpr (ERel rel@Rel{} _) = name rel
          showExpr (ERel     I{} _)   = "I"
          showExpr (ERel     V{} _)   = "V"
          showExpr (ERel rel@Mp1{} _) = "'{\tt "++relval rel++"}'"

-- add extra parentheses to consecutive superscripts, since latex cannot handle these
-- (this is not implemented in insParentheses because it is a latex-specific issue)
addParensToSuper :: Expression -> Expression
addParensToSuper e@EKl0{} = EBrk e
addParensToSuper e@EKl1{} = EBrk e
addParensToSuper e@EFlp{} = EBrk e
addParensToSuper e        = e

instance ShowMath Relation where
 showMath rel@Rel{}
  = (texOnly_Id.name) rel
 showMath I{}
  = "I"
 showMath V{}
  = "V"
 showMath r@Mp1{}
  = "'{\tt "++relval r++"}'"

instance ShowMath Declaration where
 showMath decl@(Sgn{})
  = "\\declare{"++latexEscShw(name decl)++"}{"++latexEscShw(name (source decl))++"}{"++latexEscShw(name (target decl))++"}"
 showMath Isn{}
  = "\\mathbb{I}"
 showMath Vs{}
  = "\\full"
 showMath Iscompl{}
  = "\\cmpl{\\mathbb{I}}"

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
-- To set the graphicspath, we want something like: \graphicspath{{"c:/data/ADL/output/"}}
--posixFilePath fp = "/"++System.FilePath.Posix.addTrailingPathSeparator (System.FilePath.Posix.joinPath   (tail  (splitDirectories fp)))

uniquecds :: A_Concept -> [(String,ConceptDef)]
uniquecds c = [(if length(cptdf c)==1 then cdcpt cd else cdcpt cd++show i , cd) | (i,cd)<-zip [(1::Integer)..] (cptdf c)]

makeDefinition :: Options -> Int -> String -> String -> String -> String -> [Block]
makeDefinition flags i nm lbl def ref =
  case fspecFormat flags of
    FLatex ->  [ Para ( [ RawInline "latex" $ "\\newglossaryentry{"++escapeNonAlphaNum nm ++"}{name={"++latexEscShw nm ++"}, description={"++latexEscShw def++"}}\n"] ++
                        [ RawInline "latex" $ lbl ++ "\n" | i == 0] ++
                        [ RawInline "latex" $ insertAfterFirstWord refStr defStr] ++
                        [ RawInline "latex" (latexEscShw (" ["++ref++"]")) | not (null ref) ]
                      )
               ]
    _      ->  [ Para ( Str def : [ Str (" ["++ref++"]") | not (null ref) ] )
               ]
 where refStr = "\\marge{\\gls{"++escapeNonAlphaNum nm++"}}" 
       defStr = latexEscShw def
       -- by putting the ref after the first word of the definition, it aligns nicely with the definition
       insertAfterFirstWord s wordsStr = let (fstWord, rest) = break (==' ') wordsStr
                                         in  fstWord ++ s ++ rest
 
commaEngPandoc :: Inline -> [Inline] -> [Inline]
commaEngPandoc s [a,b,c]= [a,Str ", ",b,Str ", ",s, Str " ", c]
commaEngPandoc s [a,b]  = [a,Str " ",s, Str " ", b]
commaEngPandoc _   [a]    = [a]
commaEngPandoc s (a:as) = [a, Str ", "]++commaEngPandoc s as
commaEngPandoc _   []     = []

commaNLPandoc :: Inline -> [Inline] -> [Inline]
commaNLPandoc s [a,b]  = [a,Str " ",s, Str " ", b]
commaNLPandoc  _  [a]    = [a]
commaNLPandoc s (a:as) = [a, Str ", "]++commaNLPandoc s as
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
