
module Rendering.Doc2LaTeX where
   import Rendering.Document
   import Languages
   import Strings(chain)
   import Options
   
   class ToLaTeX a where
     render2LaTeX :: Options -> a -> String

     
   instance ToLaTeX Document where
      render2LaTeX flags x = intro (dflgs x) 
                    ++ concat
                         [ "\n\\begin{document}"
                         , case language flags of
                               English -> "\n" 
                               Dutch   -> "\n\\selectlanguage{dutch}\n"
                         , "\\title{" ++ render2LaTeX flags (dtitle x) ++ "}"
                         , "\\maketitle"
                         , "\\tableofcontents"
         --                , chain "\n" [lshow language c| c<-(rd' name . preCl . Cl context . ctxwrld) context]
                         , "\n\\end{document}"
                         ]
   
          
   instance ToLaTeX DSContent where
      render2LaTeX flags x =
           case x of          
             List{} -> "\n\\begin{itemize}" 
                        ++ foldr (++) [] (map (render2LaTeX flags) (items x))
                        ++ "\n\\end{itemize}"
             Par {} -> chain "\n\n"  (map (render2LaTeX flags) (dtxts x))
             Section{} -> "\n\\" ++ subs (dsLevel x) ++ "section{"++ headtxt ++ "}"
                          ++ foldr (++) [] (map (render2LaTeX flags) (dscnts x))
                             where subs n 
                                     | n == 1 = []
                                     | n > 1  = "sub" ++ subs (n-1)
                                     | otherwise = undefined
                                   headtxt = render2LaTeX flags (dshead x)

   instance ToLaTeX DLItem where
       render2LaTeX flags item = "\n\\item " ++ render2LaTeX flags (liDTxt item)
        
   instance ToLaTeX DTxt where
     render2LaTeX _ dtxt = case dtxt of 
                           Text str -> str
                           RefSection ref -> "\n\\ref{chp:"++ref++"}"
                       


   intro :: Options -> String
   intro flags 
     = chain "\n"
         [ "\\documentclass[10pt,a4paper]{report}"
         , case language flags of
              Dutch -> "\\usepackage[dutch]{babel}" 
              English -> ""
         , "\\parskip 10pt plus 2.5pt minus 4pt  % Extra vertical space between paragraphs."
         , "\\parindent 0em                      % Width of paragraph indentation."
         , "\\usepackage{theorem}"
         , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{definition}{"
                   ++ (case language flags of
                          Dutch -> "Definitie" 
                          English -> "Definition"
                       )++"}[section]"
         , "\\theoremstyle{plain}\\theorembodyfont{\\rmfamily}\\newtheorem{designrule}[definition]{"++
                  case language flags of
                      Dutch -> "Ontwerpregel" 
                      English -> "Design Rule"
               ++"}"
         , "\\usepackage{graphicx}"
         , "\\usepackage{amssymb}"
         , "\\usepackage{amsmath}"
 --        , "\\usepackage{zed-csp}"
         , "\\usepackage{longtable}"
         , "\\def\\id#1{\\mbox{\\em #1\\/}}"
         , "\\def\\define#1{\\label{dfn:#1}{\\em #1}}"
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
         , "\\newcommand{\\fsignat}[3]{\\id{#1}:\\id{#2}\\mbox{$\\rightarrow$}\\id{#3}}"
         , "\\newcommand{\\signat}[3]{\\mbox{${#1}_{[{#2},{#3}]}$}}"
         , "\\newcommand{\\declare}[3]{\\id{#1}:\\id{#2}\\mbox{$\\times$}\\id{#3}}"
         , "\\newcommand{\\fdeclare}[3]{\\id{#1}:\\id{#2}\\mbox{$\\fun$}\\id{#3}}"]


     