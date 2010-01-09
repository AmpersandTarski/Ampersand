{-# OPTIONS_GHC -Wall #-}
 module Prototype.InterfaceDef where
  import Adl
  import Strings    (chain)
  import Data.Fspec
  import Prototype.RelBinGenBasics(isOne, commentBlock, indentBlock)
  import Version (versionbanner)
   
  interfaceDef :: Fspc -> [ObjectDef] -> String -> String
  interfaceDef fspc serviceObjects _ = "<?php\n  " ++ chain "\n  "
     (
        [ "// interfaceDef.inc.php"
        , "// Generated with "++ versionbanner
        , "// Prototype interface design by Sebastiaan JC Joosten (c) Aug 2009"
        , ""
        , "// this file contains large chunks of HTML code to improve code readability and reuse"
        , ""
        , ""
        ] ++ commentBlock [ "writeHead: code to write the page and HTML-document headers."
                          , "If extra JavaScript is needed, or to get a title,"
                          , "use the $extraheaders argument to pass extra headers"
                          ] ++
        [ "function writeHead($extraHeaders=\"\"){"
        , "  ?><!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
        , "  <HTML><HEAD>"
        , "  <script type=\"text/javascript\" src=\"jquery-1.3.2.min.js\"></script>"
        , "  <?php echo $extraHeaders; ?>"
        , "  <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />"
        , "  </HEAD><BODY STYLE=\"height:100%;width:100%;\" marginwidth=\"0\" marginheight=\"0\">"
        , "  <DIV class=\"menuDiv\"><UL class=\"menu\">"
        ] ++ indentBlock 4 menuItems ++
        [ "  </UL></DIV>"
        , "  <DIV class=\"content\">"
        , "  <!-- content -->"
        , "  <?php"
        , "}"
        , "function writeTail($buttons=\"\"){"
        , "  ?>"
        , "  <!-- tail -->"
        , "  </DIV>"
        , "  <UL class=\"buttons\">"
        , "  <!--buttons (if any)-->"
        , "  <?php echo $buttons; ?>"
        , "  </UL>"
        , "  <div class=\"cNotice\"><center><a title=\"&copy; Sebastiaan JC Joosten 2005-2009, generated with "++versionbanner++"\">Layout V1.4 alpha</A></center></div>"
        , "  </BODY></HTML><?php"
        , "}"
        , "function serviceref($svc,$env=array() ) {"
        , "  $ref = '"++name fspc++".php?content='.$svc;"
        , "  foreach($GLOBALS['ctxenv'] as $key => $value){ //CONTEXT wide variables"
        , "     $ref = $ref.'&'.$key.'='.$value;"
        , "  }"
        , "  foreach($env as $key => $value){"
        , "     $ref = $ref.'&'.$key.'='.$value;"
        , "  }"
        , "  return $ref;"
        , "}"
        , "function ifaceButton($url,$tag,$descr=\"\"){"
        , "  return '"
        , "    <LI><A HREF=\"'.$url.'\" class=\"button\" title=\"'.htmlspecialchars($descr).'\">"
        , "      '.htmlspecialchars($tag).'"
        , "    </A></LI>';"
        , "}"
        ]
     ) ++ "\n?>\n"
     where
       menuItems = concat [ [ "<LI><A HREF=\"<?php echo serviceref('"++objname++"');?>\" TITLE=\"Show all "++objname++" objects\" class=\"menuItem\" >"
                            , "  "++objname++""
                            , "</A></LI>"
                            ]
                          | o<-serviceObjects, isOne o, let objname = name o
                          ]
