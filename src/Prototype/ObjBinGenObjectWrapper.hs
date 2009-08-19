  module Prototype.ObjBinGenObjectWrapper where
--   import Char(toUpper)
   import Strings(chain)
--   import Calc( doClause)
--   import NormalForms (disjNF) --TODO -> correct replacement of Calc?
--   import ComputeRule (triggers,conjuncts,allClauses) --TODO -> correct replacement of Calc?
--   import Auxiliaries (eqCl,sort')
   import Adl
--   import ShowADL
--   import CC_aux ( tot, fun )
--   import Collection (Collection(rd))
   import Prototype.RelBinGenBasics(indentBlock,phpIdentifier,isOne,commentBlock,addToLast)
   import Data.Fspec
--   import Data.Plug
--   import Debug.Trace
   import Version (versionbanner)

   objectWrapper :: Fspc -> ObjectDef -> String
   objectWrapper fSpec o
    = chain "\n" $
      [ "<?php // generated with "++versionbanner ]
      ++ commentBlock (commentBlock ["","  Interface V1.3.1","  (c) Bas Joosten Jun 2005-Aug 2009  ","","  Using interfaceDef",""]) ++
      [ "  error_reporting(E_ALL); "
      , "  ini_set(\"display_errors\", 1);"
      , "  require \"interfaceDef.inc.php\";"
      , "  require \""++objectName++".inc.php\";"
      , "  require \"connectToDataBase.inc.php\";"
      , "  if(isset($_REQUEST['save'])) { // handle ajax save request (do not show the interface)"
      , "    $ID=@$_REQUEST['ID'];"
      , "    // we posted . characters, but something converts them to _ (HTTP 1.1 standard)"
      , "    $r=array();"
      , "    foreach($_REQUEST as $i=>$v){"
      , "      $r[join('.',explode('_',$i))]=$v; //convert _ back to ."
      , "    }"] ++
      indentBlock 4 (concat [phpList2Array 0 ("$"++phpIdentifier (name a)) (show n) a
                            | (a,n)<-zip (objats o) [(0::Integer)..]]
                    ) ++
      [ "    $"++objectId++"=new "++objectId++"($ID" ++ 
        concat [", $"++phpIdentifier (name a) | a<-objats o]++");"
      , "    if($"++objectId++"->save()) die('ok'); else die('Please fix errors!');"
      , "    exit(); // do not show the interface"
      , "  }"
      , "  $buttons=\"\";"
      , "  if(isset($_REQUEST['edit'])) $edit=true; else $edit=false;"] ++
      indentBlock 2
      ( if isOne o
        then ["$"++objectId++"=new "++objectId++"();"]
             ++ indentBlock 2 showObjectCode
             ++ [ "if(!$edit) $buttons.=ifaceButton($_SERVER['PHP_SELF'].\"?edit=1\",\"Edit\"); else"
                , "$buttons.=ifaceButton(\"JavaScript:save('\".$_SERVER['PHP_SELF'].\"?save=1');\",\"Save\");"
                ]
        else [ "if(isset($_REQUEST['"++objectId++"'])) $"++objectId++"=read"
               ++objectId++"($_REQUEST['"++objectId++"']); else $"++objectId++"=false;"
             , "if($"++objectId++"){"]
             ++ indentBlock 2 showObjectCode ++
             [ " if($edit)"
             , "   $buttons.=ifaceButton(\"JavaScript:save('\".$_SERVER['PHP_SELF'].\"?save=1',"++
                              "'\".urlencode($"++ objectId ++ "->getId()).\"');\",\"Save\")"
             , "             .ifaceButton($_SERVER['PHP_SELF'].\"?" ++ objectId ++
                              "=\".urlencode($"++objectId++"->getId()),\"Cancel\");"
             , "  else $buttons.=ifaceButton($_SERVER['PHP_SELF'].\"?edit=1&" ++ objectId ++
                              "=\".urlencode($"++objectId++"->getId()),\"Edit\")"
             , "               .ifaceButton($_SERVER['PHP_SELF'].\"?del=1&" ++ objectId ++
                              "=\".urlencode($"++objectId++"->getId()),\"Delete\");"
             , "}else{ // toon selectiescherm"
             , "  writeHead(\"<TITLE>No "++objectName++" object selected - "
                  ++ appname ++" - ADL Prototype</TITLE>\");"
             , "  $buttons.=ifaceButton($_SERVER['PHP_SELF'].\"?new=1\",\"New\");"
             , "  ?><i>No "++objectName++" object selected</i><?php "
             , "}"
             ]
      ) ++
      [ "  writeTail($buttons);"
      , "?>"
      ]
      
      where
        phpList2Array :: Int->String->String->ObjectDef->[String]
        phpList2Array depth var rqvar a
         = if Uni `notElem` multiplicities (objctx a)
           then [ var++"=array();"
                , "for($i"++show depth++"=0;isset($r['"++rqvar++".'.$i"++show depth++"]);$i"
                       ++show depth++"++){"]
                ++ indentBlock 2 (phpList2ArrayUni (depth+1)
                                                   (var++"[$i"++show depth++"]")
                                                   (rqvar++".'.$i"++show depth++".'")
                                                   a
                                 ) ++
                [ "}"]
           else if Tot `notElem` multiplicities (objctx a)
                then ["if(isset($r['"++rqvar++"'])){"]
                     ++ indentBlock 2 (phpList2ArrayUni depth var rqvar a) ++
                     ["}else "++var++"=null;"]
                else (phpList2ArrayUni depth var rqvar a)
        phpList2ArrayUni depth var rqvar a
         = addToLast ";" ([ var++" = "++head (phpList2ArrayVal var rqvar a)] ++
                          indentBlock (8 + length var) (tail (phpList2ArrayVal var rqvar a))
                         ) ++
           concat
           [ phpList2Array depth (var++"['"++phpIdentifier (name a')++"']") (rqvar++"."++show n) a'
           | (a',n)<-zip (objats a) [(0::Integer)..],Uni `notElem` multiplicities (objctx a')]
        phpList2ArrayVal :: String->String->ObjectDef->[String]
        phpList2ArrayVal var rqvar a
         = if null (objats a) then ["@$r['"++rqvar++"']"]
           else [ "array( 'id' => @$r['"++rqvar'++"']"] ++ 
                [ ", '" ++ phpIdentifier (name a') ++ "' => "
                  ++ concat (phpList2ArrayVal var (rqvar++'.':show n) a')
                | (a',n)<-zip (objats a) [(0::Integer)..], Uni `elem` multiplicities (objctx a')] ++
                [ ")"]
                 -- we gebruiken voor rqvar' liever iets waarvan het attribuut ingesteld wordt:
           where rqvar' = head ( [(rqvar++'.':show n)
                                 | (a',n)<-zip (objats a) [(0::Integer)..]
                                 , Uni `elem` multiplicities (objctx a')
                                 , isIdent (objctx a')
                                 ] ++ [rqvar] )
        objectName      = name o
        FS_id appname   = fsfsid fSpec
        objectId        = phpIdentifier objectName
        isString object = not (isOne object) -- todo
        showObjectCode
         = [ "writeHead(\"<TITLE>"++objectName++" - "++(appname)++" - ADL Prototype</TITLE>\""
           , "          .($edit?'<SCRIPT type=\"text/javascript\" src=\"edit.js\"></SCRIPT>':'<SCRIPT type=\"text/javascript\" src=\"navigate.js\"></SCRIPT>') );"
           , "if($edit)"
           , "    echo '<FORM name=\"editForm\" action=\"'"
           ,"          .$_SERVER['PHP_SELF'].'\" method=\"POST\" class=\"Edit\">';"]++
           ( if isString o
             then ["if($edit && $"++objectId++"->getId()==null)"
                  ,"     echo '<P><INPUT TYPE=\"TEXT\" NAME=\"ID\" VALUE=\"'.addslashes($"++objectId++"->getId()).'\" /></P>';"
                  ,"else echo '<H1>'.$"++objectId++"->getId().'</H1>';"
                  ,"?>"
                  ]
             else ["?><H1>"++objectName++"</H1>"]
           )
           ++ (concat [attributeWrapper (0::Integer) (show n) (if(length(objats o)>1) then "_"++phpIdentifier (name a) else "") a | (a,n)<-zip (objats o) [(0::Integer)..]])
           ++ ["<?php"
              ,"if($edit) echo '</FORM>';"
              ]
        showBlockJS cls att
         = [ "function UI"++cls++"(id){"
           , "  return " ++ head attCode'] ++ (map ((++) "       + ") (tail attCode')) ++
           [ "        ;", "}"]
           where
             attCode' = map (\x->"'"++x++"'") (attCode "'+id+'." cls att)
             attCode strt c at = ["<DIV>"++(name a)++": "++(specifics c (strt ++ show n) a)++"</DIV>"
                                 | (n,a)<-zip [(0::Integer)..] (objats at)]
             specifics c n a = if Uni `elem` multiplicities (objctx a)
                                 then if Tot `elem` multiplicities (objctx a)
                                      then "<SPAN CLASS=\"item UI"++acls c a++"\" ID=\""++n
                                           ++"\">" ++ concat (attCode n (acls c a) a) ++"</SPAN>"
                                      else "<DIV CLASS=\"new UI"++acls c a++"\" ID=\""
                                           ++n++"\"><I>Nothing</I></DIV>"
                                 else "<UL><LI CLASS=\"new UI"++acls c a
                                      ++"\" ID=\""++n++"\">new "++(name a)++"</LI></UL>"
             acls c a = c++"_"++(phpIdentifier (name a))
        attributeWrapper depth path cls att
         = [ "<DIV class=\"Floater\">"
           , "  <DIV class=\"FloaterHeader\">"++(name att)++"</DIV>"
           , "  <DIV class=\"FloaterContent\"><?php"
           , "      $"++ phpIdentifier (name att) ++" = $" ++ objectId ++ "->get_" ++ phpIdentifier (name att)++"();"
           ] ++ indentBlock 6 content ++
           [ "    ?> "
           , "  </DIV>"
           , "</DIV>"] ++
           if null newBlocks then [] else
           [ "<?php if($edit){ ?>"
           , "<SCRIPT type=\"text/javascript\">"
           , "  // code for editing blocks in "++(name att)
           ]
           ++ indentBlock 2 (concat [showBlockJS c a | (c,a)<-newBlocks]) ++
           [ "</SCRIPT>"
           , "<?php } ?>"]
           where
            (content,newBlocks)
              = (attContent ("$"++phpIdentifier (name att)) depth path cls att)
        -- attHeading shows a heading and its value
        attHeading var depth path cls att | objats att==[]
         = (["echo '"++name att++": ';"] ++ content,newBlocks)
           where (content,newBlocks) = attContent var depth path cls att
        attHeading var depth path cls att | otherwise
         = ([ "?> "
           , "<DIV class =\"Holder\"><DIV class=\"HolderHeader\">"++(name att)++"</DIV>"
           , "  <DIV class=\"HolderContent\" name=\""++(name att)++"\"><?php"
           ] ++ indentBlock 6 content ++
           [ "    ?> "
           , "  </DIV>"
           , "</DIV>"
           , "<?php"
           ],newBlocks)
           where (content,newBlocks) = attContent var depth path cls att
        -- attContent shows a list of values, using uniAtt if it is only one
        attContent var depth path cls att | Uni `notElem` multiplicities (objctx att)
         = ([ "echo '"
           , "<UL>';"
           , "foreach("++var++" as $i"++show depth++"=>"++atnm ++"){"
           , "  echo '"
           , "  <LI CLASS=\"item UI"++cls++"\" ID=\""++(path ++".'.$i"++show depth++".'")++"\">';"]
           ++ indentBlock 4 content ++
           [ "  echo '</LI>';"
           , "}"
           , "if($edit) echo '"
           , "  <LI CLASS=\"new UI"++cls++ "\" ID=\""
                              ++(path ++".'.count("++var++").'")++"\">new "++name att++"</LI>';"
           , "echo '"
           , "</UL>';"
           ], (if null (objats att) then [] else [(cls,att)])++ newBlocks)
           where
            (content,newBlocks)
             = uniAtt atnm (depth+1)
                      (path ++".'.$i"++show depth++".'") cls att
            atnm = if "$"++phpIdentifier (name att)==var then "$v"++show depth else "$"++phpIdentifier (name att)
        attContent  var depth path cls att | objats att==[]
         = ([ "echo '<SPAN CLASS=\"item UI"++cls++"\" ID=\""++path++"\">';" ]
           ++ content ++
           [ "echo '</SPAN>';" ],newBlocks)
           where (content,newBlocks) = uniAtt (var) depth path cls att
        attContent  var depth path cls att | (Tot `elem` multiplicities(objctx att))
         = ([ "echo '<DIV CLASS=\"UI"++cls++"\" ID=\""++path++"\">';" ]
           ++ indentBlock 2 content ++
           [ "echo '</DIV>';" ],newBlocks)
           where
            (content,newBlocks) = uniAtt (var) depth path cls att
        attContent  var depth path cls att | otherwise
         = ([ "if(isset("++var++")){"
            , "  echo '<DIV CLASS=\"item UI"++cls++"\" ID=\""++path++"\">';"]
            ++ indentBlock 4 content ++
            [ "  echo '</DIV>';"
            , "}else{"
            , "  echo '<DIV CLASS=\"new UI"++cls++"\" ID=\""++path++"\"><I>Nothing</I></DIV>';"
            , "}"]
           ,(if tot then [] else [(cls,att)]) ++ newBlocks
           )
           where
            (content,newBlocks) = uniAtt (var) depth path cls att
            tot = (Tot `elem` multiplicities(objctx att))
        uniAtt var _ _ _ att | objats att==[]
         = (if Tot `notElem` mults && Uni `elem` mults
           then [ "if(isset("++var++")){" ] ++ indentBlock 2 content ++ ["}"]
           else content,[])
           where content = [ "echo "++var++";"]
                 mults   = multiplicities (objctx att)
        uniAtt var depth path cls att
         = ((if null gotoPages then []
             else if length gotoPages == 1
                  then [ "if(!$edit){"
                       , "  echo '"
                       , "<A HREF=\""++(fst$head gotoPages)++"\">';"
                       , "  echo '<DIV class=\"GotoArrow\">&rarr;</DIV></A>';"
                       , "}" ]
                  else [ "if(!$edit){"
                       , "  echo '"
                       , "<DIV class=\"GotoArrow\" id=\"To"++path++"\">&rArr;</DIV>';"
                       , "  echo '<DIV class=\"Goto\" id=\"GoTo"++path++"\"><UL>';"] ++
                       [ "  echo '<LI><A HREF=\""++link++"\">"++txt++"</A></LI>';"
                       | (link,txt) <- gotoPages ] ++
                       [ "  echo '</UL></DIV>';"
                       , "}" ]
            )++
            ["echo '"
            ,"<DIV>';"]
            ++ chain ["echo '</DIV>"
                     ,"<DIV>';"] content
            ++ ["echo '"
               ,"</DIV>';"
               ,"if($edit) echo '"
               ,"<INPUT TYPE=\"hidden\" name=\""++path++".ID\" VALUE=\"'."++var++"['id'].'\" />';"
               ]
               ,newBlocks)
           where
            newBlocks = concat $ map snd stuff
            content = map fst stuff
            gotoPages :: [(String,String)]
            gotoPages
             = [ (name serv++".php?"++(phpIdentifier$name serv)++"='."++var++"['id'].'"
                 ,name serv)
               | serv<-(serviceS fSpec)
               , target (objctx serv) == target (objctx att)
               ]
            stuff
             = [ (indentBlock 2 c, b)
               | (a,n)<-zip (objats att) [(0::Integer)..]
               , (c,b)<-[attHeading (var++"['"++name a++"']") depth (path++"."++show n)
                                    (cls ++ if length(objats att) > 1
                                            then (if null cls then "" else "_")
                                                 ++ phpIdentifier (name a)
                                            else "") a
                        ]]
