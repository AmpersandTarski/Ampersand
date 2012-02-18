{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand_Prototype.InterfaceDef 
  (interfaceDef)
where
  import Data.List
  import DatabaseDesign.Ampersand_Prototype.CoreImporter
  import DatabaseDesign.Ampersand_Prototype.RelBinGenBasics(commentBlock, indentBlock)
  import DatabaseDesign.Ampersand_Prototype.RelBinGenSQL(isOne)
  import DatabaseDesign.Ampersand_Prototype.Version 
   
  interfaceDef :: Fspc -> [Interface] -> Options -> String
  interfaceDef fspc ifcs flags = intercalate "\n"
     (
        [ "<?php"
        , "// interfaceDef.inc.php"
        , "// Generated with "++ prototypeVersionStr
        , ""
        , "// this file contains large chunks of HTML code to improve code readability and reuse"
        , ""
        , ""
        ] ++ commentBlock [ "writeHead: code to write the page and HTML-document headers."
                          , "If extra JavaScript is needed, or to get a title,"
                          , "use the $extraheaders argument to pass extra headers"
                          ] ++
        [ "function writeHead($extraHeaders=\"\", $buttons=\"\"){"
        , "  ?><!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
        , "<html xmlns=\"http://www.w3.org/1999/xhtml\">"
        , "<head>"
        , "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />"
        , "<!-- jQuery -->"
        , "<link href=\"http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/base/jquery-ui.css\" rel=\"stylesheet\" type=\"text/css\"/>"
        , "<script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js\"></script>"
        , "<script src=\"http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js\"></script>"
        , "<!-- Extra Headers -->"
        , "<?php echo $extraHeaders; ?>"
        , "<!-- Screen Stylesheets -->"
        , "<link href=\"css/reset.css\"  rel=\"stylesheet\" type=\"text/css\" media=\"screen\" />"
        , "<link href=\"css/screen.css\" rel=\"stylesheet\" type=\"text/css\" media=\"screen\" />"
        , "</head>"
        , "<div id=\"container\">"
        , "  <div id=\"header\">"
    --REMARK GMI -> index.htm is eerder een beheerpagina dan een startpagina.
    --    , "    <div id=\"logo\"><a href=\"index.htm\"><img src=\"images/ampersand_logo.jpg\" width=\"165\" height=\"106\""
      --                ++ " alt=\"Ampersand logo\" title=\"Klik hier om terug te gaan naar de startpagina\" /></a></div>"
        , "    <!-- End #logo -->"
        , "    <div id=\"decoration\"></div>"
        , "    <!-- End #decoration -->"
        , "  </div>"
        , "  <!-- End #header -->"
        , "  <div id=\"menu\">"
        , "    <div class=\"primary\">"
        , "      <ul><li>"
        ] ++ indentBlock 6 menuItems ++
        (if name fspc == "ctxAtlas"
         then
          [ "      <a HREF=\"ctxAtlas.php?content=Atlas&Atlas="
                ++head([ x |c<-concs fspc,name c=="Context",x<-atomsOf c]++[error "Bug at line 52 of InterfaceDef.hs"])
                ++"\" TITLE=\"Functies die u kunt uitvoeren op de huidige context\" class=\"menuItem\" >Rapportage</a>"
          , "      <a HREF=\"ctxAtlas.php?content=Meterkast&Meterkast="++namespace flags
                ++"\" TITLE=\"Overzicht van uw eerder geladen Ampersand scripts\" class=\"menuItem\" >Bestandsoverzicht</a>"
          , "      <a HREF=\"../../index.php?file="++importfile flags
                ++"\" TITLE=\"Navigeer direct naar het geladen Ampersand script\" class=\"menuItem\" >SCRIPT</a>"
          ]
         else [])
        ++
        [ "      </li></ul>"
        , "    </div>"
        , "    <!-- End .primary -->"
        , "  </div>"
        , "  <!-- End #menu -->"
        , "  <div id=\"buttons\">"
        , "    <ul><?php echo $buttons; ?></ul>"
        , "  </div>"
        , "  <div id=\"errors\" class=\"content\"/>"
        , "  <!-- End #buttons -->"
        , "  <div id=\"content\">"
        , "<?php"
        , "}"
        , "function writeTail(){"
        ] ++
        (if name fspc=="ctxAtlas"
         then 
          ["  if ($_REQUEST['content']=='Contextoverzicht') {"
          , "    echo \"<DIV class='Floater ctxinfo'>\";"
          , "    echo \"  <DIV class='FloaterHeader'>\";"
          , "    echo \"    <DIV class='FloaterContent'>aantal relaties : "++show(length[ x |c<-concs fspc,name c=="Relation",x<-atomsOf c])++"</DIV>\";"
          , "    echo \"    <DIV class='FloaterContent'>aantal concepten: "++show(length[ x |c<-concs fspc,name c=="Concept" ,x<-atomsOf c])++"</DIV>\";"
          , "    echo \"    <DIV class='FloaterContent'>aantal regels   : "++show(length[ x |c<-concs fspc,name c=="UserRule",x<-atomsOf c])++"</DIV>\";"
          , "    echo \"  </DIV>\";"
          , "    echo \"</DIV>\";"
          , "  }"
          ]
         else [])
        ++
        [ "?>"
        , "  </div>"
        , "  <!-- End #content -->"
        , "  <div id=\"notice\">"
        , "    <span title=\"generated with "++prototypeVersionStr++"\">Layout V3.0 (Milan Interface)</span>"
        , "  </div>"
        , "  <!-- End #notice -->"
        , "<!-- ********** Javascript ********** -->"
        , "<!-- Cufon Font Replacement (http://cufon.shoqolate.com/generate)-->"
        , "<script src=\"js/cufon-yui.js\" type=\"text/javascript\"></script>"
        , "<script src=\"fonts/myriad_pro_700.font.js\" type=\"text/javascript\"></script>"
        , "<script type=\"text/javascript\">"
        , "  Cufon.replace('#menu .primary ul li a, h1, h2, .FloaterHeader, h3, h4, h5, h6', {"
        , "    fontFamily: 'Myriad Pro',"
        , " hover: 'true'"
        , "  });"
        , "</script>"
        , "<!-- jQuery -->"
        , "<script type=\"text/javascript\">"
        , "$(\".GotoArrow\").hover("
        , "  function () {"
        , " $(this).addClass(\"GotoArrowHover\");"
        , "  },"
        , ""
        , "  function () {"
        , " $(this).removeClass(\"GotoArrowHover\");"
        , "  }"
        , ");"
        , "</script>"
        , "</body>"
        , "</html>"
        , "<?php"
        , "}"
        , "function interfaceref($ifc,$new=false,$edit=false,$env=array() ) {"
        , "  $ref = '"++name fspc++".php?content='.$ifc;"
        , "    if ($new) $ref=$ref.'&new=1';"
        , "    elseif ($edit) $ref=$ref.'&edit=1';"
        , "  if (isset($GLOBALS['ctxenv'])){"
        , "   foreach($GLOBALS['ctxenv'] as $key => $value){ //CONTEXT wide variables"
        , "     $ref = $ref.'&'.$key.'='.$value;"
        , "  }}"
        , "  foreach($env as $key => $value){"
        , "     $ref = $ref.'&'.$key.'='.$value;"
        , "  }"
        , "  return $ref;"
        , "}"
        , "function ifaceButton($url,$tag,$descr=\"\"){"
        , "return '"
        , "  <li><a href=\"'.$url.'\" class=\"button\" title=\"'.htmlspecialchars($descr).'\">"
        , "      '.htmlspecialchars($tag).'</a></li>';"
        , "}"
        , "function savevalue($val){"
        , "  if (!isset($val) || $val=='Nothing') return null; else return $val;"
        , "}"
        , "?>"
        ]
     )
     where
     menuItems 
       = concat [ [ "<a href=\""++ifcref
                , "  "++name o++""
                , "</a>"
                ]
                | ifc<-ifcs, let o=ifcObj ifc
                , isOne o
                , not (genAtlas flags) || elem (name o) ["Contextoverzicht", "Overtredingen","Regels","Relaties","Paren","Concepten","Termen"]
                , let ifctitle = case language flags of Dutch -> "Toon alle "++name ifc; English -> "Show all " ++name ifc
                , let ifcref="<?php echo interfaceref('"++name ifc++"');?>\" TITLE=\""++ifctitle++"\" class=\"menuItem\">"
                ]
