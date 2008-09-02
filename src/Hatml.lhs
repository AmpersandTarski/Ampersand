> module Hatml where
>  import Char (isAlphaNum, isSpace)
>  import CommonClasses ( Identified(name) )
>  import Auxiliaries
>         ( chain, sort') 
>  import ADLdef
>  import CC_aux  ( Concept, Morphism, isIdent, isNot)

Basic HTML markup

>  htmlItalic str = "<I>"++str++"</I>"
>  htmlBold str   = "<B>"++str++"</B>"
>  htmlOverline str = "<SPAN STYLE=\"TEXT-DECORATION:overline\">"++str++"</SPAN>"

>  htmlFline (nm,src)
>   = "<FRAME Name="++show nm++" SRC="++show (htmlname src++".html")++" NORESIZE FRAMEBORDER=\"No\">"

>  htmlPage title head body
>   = chain "\n" (["<HTML>"]++[htmlHead title head]++[body|not(null body)]++["</HTML>"])

>  htmlHead title head
>   = chain "\n"
>     ([ "<HEAD>"
>      , "<META name=\"author\" content=\"S.Joosten\">"
>      , "<META name=\"email\" content=\"stef.joosten@ou.nl\">" ] ++
>      [ "<TITLE>"++title++"</TITLE>" | not (null title) ]            ++
>      [ head | not (null head) ]                                     ++
>      [ "</HEAD>"])

>  htmlBody body
>   = chain "\n" ["<BODY bgcolor=#FFFFBB link=#AA0000 alink=#AA0000 hlink=#AA0000>",body,"</BODY>"]

>  htmlImageObsolete imgname
>   = "<IMG src=\""++imgname++"\" BORDER=\"0\" VSPACE=\"0\" HSPACE=\"0\" />"

>  htmlImageMap imgname mapname mapcontents
>   = chain "\n"
>        [ "<IMG src=\""++imgname++"\" USEMAP=\"#"++mapname++"\" BORDER=\"0\" VSPACE=\"0\" HSPACE=\"0\" />"
>    --     ,"<MAP NAME=\""++mapname++"\">"        DEZE REGEL MOET WEG, ZIT AL IN mapcontents
>         , mapcontents
>    --     ,"</MAP>"                              EN DEZE REGEL MOET DUS OOK WEG
>        ]

>  htmlDotted ls
>   = chain "\n" (["<UL>"]++["<LI>"++l++"</LI>"|l<-ls]++["</UL>"])

>  htmlNumbered ls
>   = chain "\n" (["<OL>"]++["<LI>"++l++"</LI>"|l<-ls]++["</OL>"])

>  htmlValNumbered ls
>   = chain "\n" (["<OL>"]++["<LI VALUE=\""++show nr++"\">"++l++"</LI>"|(nr,l)<-sort' fst ls]++["</OL>"])

>  htmlAnchor url clicktext params = "<A HREF="++url++concat [" "++p|p<-params]++">"++clicktext++"</A>"

>  htmlHeadinglevel n str params = "<H"++show n++concat [" "++p|p<-params]++">"++str++"</H"++show n++">\n<A NAME=\"REF"++show n++str++"\"></A>\n"

>  htmlname str = [if isAlphaNum c then c else if c `elem` "/\\" then c else '_'| c<-str]

htmlDropDown creates drop-down boxes in HTML.

>  htmlDropDown title params options
>   = "  <SELECT NAME="++show title++ concat [" "++p|p<-params]++">"++
>     concat ["\n    <OPTION "++fn++"> "++n|(fn,n)<-options]++
>     "\n  </SELECT>"

Example:
 htmlDropDown "SwitchPattern" ["onchange=\"SwitchPattern(this.value)\""] [("value=\"Basics_Set.html\"">, "Set")]
yields
  <SELECT NAME="SwitchPattern" onChange="SwitchPattern(this.value)">
    <OPTION value="Basics_Set.html"> Set
  </SELECT>

>{-  htmlSTable [] = ""
>  htmlSTable (xs:xxs)
>   = chain "\n"
>     ( ["<TABLE>"]++
>        [ "<TR><td width=1 bgcolor=black rowspan="++show(1+length xxs)++"><img src=\"spacer.gif\"width=1></td>"++
>          concat [ "<TD width="++show p++"% bgcolor=black><b><font color=#FFFFFF>"++x++
>                   "</font></b></TD><td width=1 bgcolor=black rowspan="++show(1+length xxs)++
>                   "><img src=\"spacer.gif\" width=1></td>"
>                 | (p,x)<-zip [40,20,20,20,20] xs]++"</TR>"]++
>       ["<TR>"++concat ["<TD valign=top>"++x++"</TD>"|x<-xs]++"</TR>"|xs<-if null xxs then [] else init xxs]++
>       ["<TR>"++concat ["<TD valign=top>"++x++"</TD>"|x<-if null xxs then [] else last xxs]++"</TR>"]++
>       ["<TR><TD colspan="++show(1+2*length xs)++" bgcolor=black><img src=\"spacer.gif\" height=1></TD></TR>"]++["</TABLE>"])-}

>  htmlTable xxs opts = chain "\n" (["<TABLE"++(if null opts then "" else " "++opts)++">"]++["<TR>"++concat ["<TD valign=top>"++x++"</TD>"|x<-xs]++"</TR>"|xs<-xxs]++["</TABLE>"])

Risk: the following way of avoiding JavaScript reserved words is not entirely safe! It works well, though.

>  avoidJSreservedwords str = "JS_"++str

>  minispace x = if null x || and(map isSpace x) then "&nbsp;" else x

htmlSortTable is currently not used:

>  htmlSortTable fn i color xs xxs
>   = chain "\n"
>     ([ "<TABLE WIDTH=\"75%\" BORDER=1 CELLSPACING=1 CELLPADDING=1 name=\"rsTable\" id=rsTable  cols="++show(length xs)++">"]++
>      [ "<TR bgcolor="++color++">" ]++
>      [ "  <TD>"++(if i==c then attr else htmlAnchor (fn++show c++".html") attr [])++"</TD>"
>      | (c,a)<-zip [0..] xs, attr<-["<FONT color=white>"++htmlBold a++"</FONT>"]]++
>      [ "</TR>" ]++
>      [ "<TR>"++concat ["<TD>"++minispace x++"</TD>"|x<-xs]++"</TR>" | xs<-xxs ]++
>      [ "</TABLE>" ])

Colors are "darkred" for tables containing relations and "red" for tables showing violations.

>  htmlBlueTable color as xxs
>   = concat
>     ([ "<TABLE WIDTH=\"75%\" BORDER=1 CELLSPACING=1 CELLPADDING=1 name=\"rsTable\" id=rsTable  cols="++show(length as)++">"]++
>      [ "\n<TR bgcolor="++color++">" ]++
>      [ "\n  <TD><FONT color=white>"++htmlBold a++"</FONT></TD>" | a<-as]++
>      [ "</TR>\n<TR><TD> <SPAN style=\"visibility:hidden;position:absolute\">&nbsp;&nbsp;= [ (\"</SPAN>" 
>        ++ chain "<SPAN style=\"visibility:hidden;position:absolute\">\")</SPAN></TD></TR>\n<TR><TD><SPAN style=\"visibility:hidden;position:absolute\">&nbsp;&nbsp;&nbsp;&nbsp;; (\"</SPAN>" [ chain "<SPAN style=\"visibility:hidden;position:absolute\">\",</SPAN></TD><TD><SPAN style=\"visibility:hidden;position:absolute\">\"</SPAN>" [minispace x|x<-xs] | xs<-xxs ]] ++
>      [ "<SPAN style=\"visibility:hidden;position:absolute\">\")<BR />&nbsp;&nbsp;&nbsp;&nbsp;]</SPAN></TD></TR></TABLE>" ])

htmlTable' is currently not used:

>  htmlTable' color t xs
>   = chain "\n"
>     ([ "<TABLE WIDTH=\"75%\" BORDER=1 CELLSPACING=1 CELLPADDING=1 name=\"rsTable\" id=rsTable  cols=1>"]++
>      [ "<TR bgcolor="++color++"><TD><FONT color=white>"++htmlBold t++"</FONT></TD></TR>" ]++
>      [ "<TR><TD>"++minispace x++"</TD></TR>" | x<-xs ]++
>      [ "</TABLE>" ])

The following requires data structures from CC_aux

>  class HTML a where
>   hshow :: a -> String

>  instance HTML Concept where
> --XXXVervangen:  hshow (C nm _ _) = htmlAnchor (htmlname nm++".html") nm []
>   hshow c = htmlAnchor (htmlname (name c)++".html") (name c) []

>  instance HTML Morphism where
>   hshow m | isIdent m = "="
>           | isNot m   = "&neq;"
>           | otherwise = name m

