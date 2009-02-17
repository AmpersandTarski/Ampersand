-- | This module contains layout rules for LaTeX. Modules that consume this 
--   module should not use any LaTeX stuff themselves. 
module LaTeX 
where
   import Char
   import Strings
  
   wrapMath str = "$"++str++"$"

   
   tt :: String -> String
   tt a = "{\\tt "++a++"}"


   latexCenter = latex "center" []
   latexFigure = latex "figure" ["[htb]"]
   latexFigureHere = latex "figure" ["[h]"]

   latex :: String -> [String] -> String -> String
   latex command params content
    = "\\begin{"++command++"}"++chain "," params++"\n"++content++"\n\\end{"++command++"}"

   latexSubsection title reference
    = "\n\\subsection*{"++title++"}\n" ++"\\label{ssct:"++reference++"}\n"

   latexSection title reference
    = "\n\\section{"++title++"}\n" ++"\\label{sct:"++reference++"}\n"

   latexChapter title reference
    = "\n\\chapter{"++title++"}\n" ++"\\label{chp:"++reference++"}\n"

   latexDotted ls
    = if null ls then "" else
      "\n"++chain "\n" (["\\begin{itemize}"]++["\\item "++l|l<-ls]++["\\end{itemize}"])

   latexEnumerate ls
    = if null ls then "" else
      chain "\n" (["\\begin{enumerate}"]++["\\item "++l|l<-ls]++["\\end{enumerate}"])

   latexDefinition nm [] = ""
   latexDefinition nm [def]
    = chain "\n" ["\\begin{definition}{"++latexWord nm++"\\\\}", def, "\\end{definition}"]
   latexDefinition nm defs
    = chain "\n" (["\\begin{definition}{"++latexWord nm++"}","  \\begin{itemize}"]++["  \\item "++l|l<-defs]++["  \\end{itemize}","\\end{definition}"])

   latexDesignrule str
    = chain "\n" ["\\begin{designrule}{}", str, "\\end{designrule}"]

   latexEmph x = "\\emph{"++filter isAlphaNum x++"}"

   latexWord :: String -> String
   latexWord = addSlashes
     where
       addSlashes (' ': '\"': cs) = " ``"++addSlashes cs
       addSlashes ('\"': ' ': cs) = "'' "++addSlashes cs
       addSlashes ('\\': cs) = "\\\\"++addSlashes cs
       addSlashes ('_': cs) = "\\_"++addSlashes cs
       addSlashes ('&': cs)  = "\\&"++addSlashes cs        -- HJO, 20 dec 2008:
       addSlashes ('é': cs) = "\\'e"++addSlashes cs       --TODO: LaTeX extentie inbouwen. Unicode moet gewoon werken in LaTeX: Zie http://gunnarwrobel.de/wiki/Unicode.html of nog beter: http://en.wikibooks.org/wiki/LaTeX/Internationalization
       addSlashes ('è': cs) = "\\`e"++addSlashes cs       --      Anders is hiernaast een manier om unicode te gebruiken in Haskell. Wat een gedoe!
       addSlashes ('ë': cs) = "\\\"e"++addSlashes cs
       addSlashes ('ï': cs) = "\\\"{\\i}"++addSlashes cs
       addSlashes ('á': cs) = "\\'a"++addSlashes cs
       addSlashes ('à': cs) = "\\`a"++addSlashes cs
       addSlashes ('ó': cs) = "\\'o"++addSlashes cs
       addSlashes ('ò': cs) = "\\`o"++addSlashes cs
     --  addSlashes ('``': cs) = "\\``"++addSlashes cs
     --  addSlashes ('''': cs) = "\\''"++addSlashes cs
       addSlashes (c: cs)    = if ord c>127 then error("Character '"++[c]++"' (ASCII "++show (ord c)++") is not mapped correctly to LaTeX by ADL in \""++c:cs++"\".") else
                               c:addSlashes cs
       addSlashes _          = ""


