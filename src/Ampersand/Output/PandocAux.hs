{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.PandocAux
      ( writepandoc
      , Chapter(..), chptTitle
      , count
      , ShowMath(..),showMathWithSign
      , latexEscShw, escapeNonAlphaNum
      , texOnlyId
      , texOnlyMarginNote
      , newGlossaryEntry
      , NLString(..)
      , ENString(..)
      , LocalizedStr
      , localize
      , commaPandocAnd
      , commaPandocOr
      , Inlines
      )
where
import Control.Monad
import Data.Char hiding    (Space)
import Data.List
import Data.Maybe
import Ampersand.ADL1
import Ampersand.Basics hiding (hPutStrLn)
import Ampersand.FSpec
import Ampersand.Misc
import Ampersand.Core.ShowPStruct
import Ampersand.Classes (isFunction)
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
import Text.Pandoc.MediaBag
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
                                            else "Functioneel Ontwerp van "
                        (English, False) -> "Functional Design of "
                        (Dutch  ,  True) -> "Diagnose van "
                        (English,  True) -> "Diagnosis of "
                )++name fSpec)
 --   , ("mainfont",
 --   , ("sansfont",
 --   , ("monofont",
 --   , ("mathfont",
    , ("fontsize", "12pt")   --can be overridden by geometry package (see below)
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
         , "% caption – Customising captions in floating environments"
         , "\\usepackage{caption}"
         , "\\captionsetup{format=plain"
         , "              ,textfont=bf,labelfont=small"
         , "              ,labelsep=none"
         , "              ,labelformat=empty"
         , "              ,width=.85\\textwidth"
         , "              }"
         , ""
         , "% textcomp – LATEX support for the Text Companion fonts -- Disabled because obsolete."
         , "% \\usepackage{textcomp}"
         , ""
         , "% hypcap – Adjusting the anchors of captions"
         , "\\usepackage[all]{hypcap}"
         , ""
         -- , "% adaptation1) For the purpose of clear references in Latex. See also https://github.com/AmpersandTarski/ampersand/issues/31"
         -- , "\\makeatletter"
         -- , "\\let\\orgdescriptionlabel\\descriptionlabel"
         -- , "\\renewcommand*{\\descriptionlabel}[1]{%"
         -- , "  \\let\\orglabel\\label"
         -- , "  \\let\\label\\@gobble"
         -- , "  \\phantomsection"
         -- , "  \\edef\\@currentlabel{#1}%"
         -- , "  %\\edef\\@currentlabelname{#1}%"
         -- , "  \\let\\label\\orglabel"
         -- , "  \\orgdescriptionlabel{#1}%"
         -- , "}"
         -- , "\\makeatother"
         -- , "% End-adaptation1"
         , ""
         , "% adaptation2) The LaTeX commands \\[ and \\], are redefined in the amsmath package, making sure that equations are"
         , "% not numbered. This is undesireable behaviour. this is fixed with the following hack, inspired on a note"
         , "% found at http://tex.stackexchange.com/questions/40492/what-are-the-differences-between-align-equation-and-displaymath"
         , "\\DeclareRobustCommand{\\[}{\\begin{equation}}"
         , "\\DeclareRobustCommand{\\]}{\\end{equation}}"
         , "% End-adaptation2"
         , ""
         , ""
         , "% ============Ampersand specific End==================="
         ])
    | fspecFormat (getOpts fSpec) == FLatex ]

--DESCR -> functions to write the pandoc
--         String = the name of the outputfile
--         The IO() creates the actual output
writepandoc :: FSpec -> Pandoc -> IO()
writepandoc fSpec thePandoc = 
  do verboseLn (getOpts fSpec) ("Generating "++fSpecFormatString++" to : "++outputFile)
     media <- collectMedia
     verboseLn (getOpts fSpec) "Media collected"
     let wOpts = writerOptions media
     writeToFile wOpts
     -- In case of Latex, we need to postprocess the .ltx file to pdf:
     case fspecFormat (getOpts fSpec) of
        FLatex  ->
           do result <- makePDF writeLaTeX wOpts thePandoc fSpec
              case result of 
                Left err -> do putStrLn "LaTeX Error: "
                               B.putStr err
                               putStrLn "\n."
                Right _  -> do let pdfFile = outputFile -<.> "pdf"
                               verboseLn (getOpts fSpec) $ pdfFile ++" created."
             
        _ -> return ()
 where
    writeToFile :: WriterOptions -> IO()
    writeToFile wOpts = do
      when (fspecFormat (getOpts fSpec) == Fdocx)
           writeReferenceFileDocx
      case getWriter fSpecFormatString of
        Left msg -> fatal 162 . unlines $
                        ("Something wrong with format "++show(fspecFormat (getOpts fSpec))++":")
                        : map ("  "++) (lines msg)
        Right (PureStringWriter worker)   -> do let content = worker wOpts thePandoc
                                                writeFile outputFile content
        Right (IOStringWriter worker)     -> do content <- worker wOpts thePandoc
                                                writeFile outputFile content
        Right (IOByteStringWriter worker) -> do content <- worker wOpts thePandoc
                                                BC.writeFile outputFile content

    outputFile = 
      addExtension (dirOutput (getOpts fSpec) </> baseName (getOpts fSpec))
                   (case fspecFormat (getOpts fSpec) of
                      Fasciidoc     -> ".txt"
                      Fcontext      -> ".context"
                      Fdocbook      -> ".docbook"
                      Fdocx         -> ".docx"
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
    fSpecFormatString :: String
    fSpecFormatString =
       case fspecFormat (getOpts fSpec) of
            FPandoc   -> "pandoc"
            Fasciidoc -> "asciidoc"
            Fcontext  -> "context"
            Fdocbook  -> "docbook"
            Fdocx     -> "docx"
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
    writerOptions :: MediaBag-> WriterOptions
    writerOptions bag = def
                      { writerTableOfContents=True
                      , writerNumberSections=True
                      , writerTemplate=template
                      , writerVariables=defaultWriterVariables fSpec
                      , writerMediaBag=bag
                      , writerReferenceDocx=Just docxStyleUserPath
                      , writerVerbose=verboseP (getOpts fSpec)
                      }
     where template  = getStaticFileContent PandocTemplates ("default."++fSpecFormatString)
    docxStyleContent :: BC.ByteString 
    docxStyleContent = 
      case getStaticFileContent PandocTemplates "defaultStyle.docx" of
         Just cont -> BC.pack cont
         Nothing -> fatal 0 "Cannot find the statically included default defaultStyle.docx."
    docxStyleUserPath = dirOutput (getOpts fSpec) </> "reference.docx" -- this is the place where the file is written if it doesn't exist.
    writeReferenceFileDocx :: IO()
    writeReferenceFileDocx = do
         exists <- doesFileExist docxStyleUserPath
         if exists 
             then verboseLn (getOpts fSpec) 
                      "Existing style file is used for generating .docx file:"
             else (do verboseLn (getOpts fSpec)
                           "Default style file is written. this can be changed to fit your own style:"
                      BC.writeFile docxStyleUserPath docxStyleContent
                  )
         verboseLn (getOpts fSpec) docxStyleUserPath
    collectMedia :: IO MediaBag
    collectMedia = do files <- listDirectory . dirOutput . getOpts $ fSpec
                      let graphicsForDotx = map (dirOutput (getOpts fSpec) </>) . filter isGraphic $ files 
                      foldM addToBag mempty graphicsForDotx                                  
       where addToBag :: MediaBag -> FilePath -> IO MediaBag
             addToBag bag fullPath = do
                verboseLn (getOpts fSpec) $ "Collect: "++fullPath
                verboseLn (getOpts fSpec) $ "  as: "++takeFileName fullPath
                contents <- BC.readFile fullPath
                return $ insertMedia (takeFileName fullPath) Nothing contents bag 
             isGraphic :: FilePath -> Bool
             isGraphic f = takeExtension f `elem` [".svg"]

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

data Chapter = Intro
             | SharedLang
             | Diagnosis
             | ConceptualAnalysis
             | ProcessAnalysis
             | DataAnalysis
             | SoftwareMetrics
             | Interfaces
             | FunctionPointAnalysis
             | Glossary
             deriving (Eq, Show)


chptTitle :: Lang -> Chapter -> Inlines
chptTitle lang cpt =
  case cpt of
    Intro                 -> text.l $ (NL "Inleiding", EN "Introduction")
    SharedLang            -> text.l $ (NL "Gemeenschappelijke taal", EN "Shared Language")
    Diagnosis             -> text.l $ (NL "Diagnose", EN "Diagnosis")
    ConceptualAnalysis    -> text.l $ (NL "Conceptuele Analyse", EN "Conceptual Analysis")
    ProcessAnalysis       -> text.l $ (NL "Procesanalyse", EN "Process Analysis")
    DataAnalysis          -> text.l $ (NL "Gegevensstructuur", EN "Data structure")
    SoftwareMetrics       -> text.l $ (NL "Functiepunt Analyse", EN "Function Point Analysis")
    Interfaces            -> text.l $ (NL "Koppelvlakken", EN "Interfaces")
    FunctionPointAnalysis -> text.l $ (NL "Functiepuntanalyse", EN "Function point analysis")
    Glossary              -> text.l $ (NL "Begrippen", EN "Glossary")
 where 
     -- shorthand for easy localizing    
    l :: LocalizedStr -> String
    l = localize lang
    



--DESCR -> pandoc print functions for Ampersand data structures
---------------------------------------
-- LaTeX math markup
---------------------------------------

class ShowMath a where
 showMath :: a -> Inlines

instance ShowMath Rule where
 showMath = showMath . rrexp

instance ShowMath Expression where
 showMath = math . showExpr . insParentheses
   where  showExpr (EEqu (l,r)) = showExpr l++inMathEquals++showExpr r
          showExpr (EInc (l,r)) = showExpr l++inMathInclusion++showExpr r
          showExpr (EIsc (l,r)) = showExpr l++inMathIntersect++showExpr r
          showExpr (EUni (l,r)) = showExpr l++inMathUnion++showExpr r
          showExpr (EDif (l,r)) = showExpr l++inMathDifference ++showExpr r
          showExpr (ELrs (l,r)) = showExpr l++inMathLeftResidu++showExpr r
          showExpr (ERrs (l,r)) = showExpr l++inMathRightResidu++showExpr r
          showExpr (EDia (l,r)) = showExpr l++inMathDiamond++showExpr r
          showExpr (ECps (EEps i sgn,r)) | i==source sgn||i==target sgn = showExpr  r
                                         | otherwise                    = showExpr (ECps (EDcI i,r))
          showExpr (ECps (l,EEps i sgn)) | i==source sgn||i==target sgn = showExpr  l
                                         | otherwise                    = showExpr (ECps (l,EDcI i))
          showExpr (ECps (l,r)) = showExpr l++inMathCompose++showExpr r
          showExpr (ERad (l,r)) = showExpr l++inMathRelativeAddition++showExpr r
          showExpr (EPrd (l,r)) = showExpr l++inMathCartesianProduct++showExpr r
          showExpr (EKl0 e)     = showExpr (addParensToSuper e)++inMathStar
          showExpr (EKl1 e)     = showExpr (addParensToSuper e)++inMathPlus
          showExpr (EFlp e)     = showExpr (addParensToSuper e)++inMathFlip
          showExpr (ECpl e)     = "\\overline{"++showExpr e++"}"
          showExpr (EBrk e)     = "("++showExpr e++")"
          showExpr (EDcD d)     = inMathText (name d)
          showExpr (EDcI c)     = "I_{["++inMathText (name c)++"]}"
          showExpr  EEps{}      = "" -- fatal 417 "EEps may occur only in combination with composition (semicolon)."  -- SJ 2014-03-11: Are we sure about this? Let's see if it ever occurs...
          showExpr (EDcV sgn)   = "V_{["++inMathText (name (source sgn))++"*"++inMathText (name (target sgn))++"]}"
          showExpr (EMp1 val _) = inMathText $ showP val

-- add extra parentheses to consecutive superscripts, since latex cannot handle these
-- (this is not implemented in insParentheses because it is a latex-specific issue)
addParensToSuper :: Expression -> Expression
addParensToSuper e@EKl0{} = EBrk e
addParensToSuper e@EKl1{} = EBrk e
addParensToSuper e@EFlp{} = EBrk e
addParensToSuper e        = e

instance ShowMath Declaration where
 showMath decl@Sgn{}  = math $ 
        inMathText (name decl)++":\\ "
     ++(inMathText . name . source $ decl)++(if isFunction decl then "\\mapsto" else "*")
     ++(inMathText . name . target $ decl)++"]"
 showMath Isn{}
  = math "\\mathbb{I}"
 showMath Vs{}
  = math "\\mathbb{V}"
showMathWithSign :: Declaration -> Inlines
showMathWithSign decl = math $ 
        inMathText (name decl)++"["
     ++(inMathText . name . source $ decl)++"*"
     ++(inMathText . name . target $ decl)++"]"
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
--- Math related stuff ---
---------------------------
-- safe function to have plain text in a piece of Math
inMathText :: String -> String
inMathText s = "\\text{"++latexEscShw s++"} "

inMathCartesianProduct :: String
inMathCartesianProduct = "\\times "

texOnlyId :: String -> String
texOnlyId s = "\\mbox{"++latexEscShw s++"} "
-- \\def\\id#1{\\mbox{\\em #1\\/}}"

inMathCompose :: String
inMathCompose = ";"

inMathRelativeAddition :: String
inMathRelativeAddition = "\\dagger "

inMathIntersect :: String
inMathIntersect = "\\cap "

inMathUnion :: String
inMathUnion = "\\cup "

inMathInclusion :: String
inMathInclusion = "\\vdash "

inMathEquals :: String
inMathEquals = "="

inMathStar :: String
inMathStar = "^{*}"

inMathPlus :: String
inMathPlus = "^{+}"

inMathDifference :: String
inMathDifference = " - "

inMathLeftResidu :: String
inMathLeftResidu = " / "

inMathRightResidu :: String
inMathRightResidu = " \\backslash "

inMathDiamond :: String
inMathDiamond = " \\Diamond "

inMathFlip :: String
inMathFlip = "^{\\smallsmile}"

newGlossaryEntry :: String -> String -> Inlines
newGlossaryEntry nm cnt =
  rawInline "latex"
    ("\\newglossaryentry{"++escapeNonAlphaNum nm ++"}\n"++
     "     { name={"++latexEscShw nm ++"}\n"++
     "     , description={"++latexEscShw cnt++"}}\n")

texOnlyMarginNote :: String -> String
texOnlyMarginNote mgn = 
   "\\marginpar{\\begin{minipage}[t]{3cm}{\\noindent\\small\\em "++mgn++"}\\end{minipage}}"

-------------------------------------------------
---temporary from Pandoc:
------------------------------------------

makePDF :: (WriterOptions -> Pandoc -> String)  -- ^ writer
        -> WriterOptions       -- ^ options
        -> Pandoc              -- ^ document
        -> FSpec
        -> IO (Either B.ByteString B.ByteString)
makePDF writer wOpts pandoc fSpec = 
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

commaPandocAnd :: Lang -> [Inlines] -> Inlines
commaPandocAnd Dutch = commaNLPandoc "en"
commaPandocAnd English = commaEngPandoc "and"
commaPandocOr :: Lang -> [Inlines] -> Inlines
commaPandocOr Dutch = commaNLPandoc "of"
commaPandocOr English = commaEngPandoc "or"

commaEngPandoc :: Inlines -> [Inlines] -> Inlines
commaEngPandoc s [a,b,c] = a <> ", " <> b <> ", " <> s <> space <> c
commaEngPandoc s [a,b]   = a <> space <> s <> space <> b
commaEngPandoc _   [a]   = a
commaEngPandoc s (a:as)  = a <> ", " <> commaEngPandoc s as
commaEngPandoc _   []    = mempty

commaNLPandoc :: Inlines -> [Inlines] -> Inlines
commaNLPandoc s [a,b]  = a <> space <> s <> space <> b
commaNLPandoc  _  [a]  = a
commaNLPandoc s (a:as) = a <> ", " <> commaNLPandoc s as
commaNLPandoc  _  []   = mempty
   
